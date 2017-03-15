/* @internal */
namespace ts.codefix.extractMethod {
    export type RangeToExtract = Expression | Statement[];
    export type Scope = FunctionLikeDeclaration | SourceFile | ModuleBlock | ClassDeclaration | ClassExpression;
    export interface ExtractResultForScope {
        readonly scope: Scope;
        readonly changes: FileTextChanges[];
    }

    export function getRangeToExtract(sourceFile: SourceFile, span: TextSpan): RangeToExtract | undefined {
        const start = getParentNodeInSpan(getTokenAtPosition(sourceFile, span.start), sourceFile, span);
        const end = getParentNodeInSpan(findTokenOnLeftOfPosition(sourceFile, textSpanEnd(span)), sourceFile, span);
        if (!start || !end || start.parent !== end.parent) {
            return undefined;
        }
        if (start !== end) {
            // start and end should be statements and parent should be either block or a source file
            if (!isBlockLike(start.parent)) {
                return undefined;
            }
            if (!(isStatement(start) || isExpression(start)) && !(isStatement(end) || isExpression(end))) {
                return undefined;
            }
            const statements: Statement[] = [];
            for (const n of (<BlockLike>start.parent).statements) {
                if (n === start || statements.length) {
                    if (!canExtractNode(n)) {
                        return undefined;
                    }
                    statements.push(n);
                }
                if (n === end) {
                    break;
                }
            }
            return statements;
        }
        else {
            if (!canExtractNode(start)) {
                return undefined;
            }
            if (isStatement(start)) {
                return [start];
            }
            else if (isExpression(start)) {
                return start;
            }
            else {
                return undefined;
            }
        }

        function canExtractNode(n: Node): boolean {
            const enum PermittedJumps {
                None = 0,
                Break = 1 << 0,
                Continue = 1 << 1,
                Return = 1 << 2,
                Yield = 1 << 3,
                Await = 1 << 4
            }

            let canExtract = true;
            let permittedJumps = PermittedJumps.Return | PermittedJumps.Yield | PermittedJumps.Await;
            let seenLabels: string[];
            visit(n);
            return canExtract;

            function visit(n: Node) {
                if (!canExtract || !n || isFunctionLike(n) || isClassLike(n)) {
                    return;
                }
                const savedPermittedJumps = permittedJumps;
                if (n.parent) {
                    switch (n.parent.kind) {
                        case SyntaxKind.IfStatement:
                            if ((<IfStatement>n.parent).thenStatement === n || (<IfStatement>n.parent).elseStatement === n) {
                                // forbid all jumps inside thenStatement or elseStatement 
                                permittedJumps = PermittedJumps.None;
                            }
                            break;
                        case SyntaxKind.TryStatement:
                            if ((<TryStatement>n.parent).tryBlock === n || (<TryStatement>n.parent).finallyBlock === n) {
                                // forbid all jumps inside try or finally blocks
                                permittedJumps = PermittedJumps.None;
                            }
                            break;
                        case SyntaxKind.CatchClause:
                            if ((<CatchClause>n.parent).block === n) {
                                // forbid all jumps inside the block of catch clause
                                permittedJumps = PermittedJumps.None;
                            }
                            break;
                        case SyntaxKind.CaseClause:
                            if ((<CaseClause>n).expression !== n) {
                                // allow unlabeled break inside case clauses
                                permittedJumps |= PermittedJumps.Break;
                            }
                            break;
                        default:
                            if (isIterationStatement(n.parent, /*lookInLabeledStatements*/ false)) {
                                if ((<IterationStatement>n.parent).statement === n) {
                                    // allow unlabeled break/continue inside loops
                                    permittedJumps |= PermittedJumps.Break | PermittedJumps.Continue;
                                }
                            }
                            break;
                    }
                }
                switch (n.kind) {
                    case SyntaxKind.LabeledStatement:
                        {
                            const label = (<LabeledStatement>n).label;
                            (seenLabels || (seenLabels = [])).push(label.text);
                            forEachChild(n, visit);
                            seenLabels.pop();
                            break;
                        }
                    case SyntaxKind.BreakStatement:
                    case SyntaxKind.ContinueStatement:
                        {
                            const label = (<BreakStatement | ContinueStatement>n).label;
                            if (label) {
                                if (!contains(seenLabels, label.text)) {
                                    // attempts to jump to label that is not in range to be extracted
                                    canExtract = false;
                                }
                            }
                            else {
                                if (!(permittedJumps & (SyntaxKind.BreakStatement ? PermittedJumps.Break : PermittedJumps.Continue))) {
                                    // attempt to break or continue in a forbidden context
                                    canExtract = false;
                                }
                            }
                            break;
                        }
                    case SyntaxKind.AwaitExpression:
                        if (!(permittedJumps & PermittedJumps.Await)) {
                            canExtract = false;
                        }
                        break;
                    case SyntaxKind.YieldExpression:
                        if (!(permittedJumps & PermittedJumps.Yield)) {
                            canExtract = false;
                        }
                        break;
                    case SyntaxKind.ReturnStatement:
                        if (!(permittedJumps & PermittedJumps.Return)) {
                            canExtract = false;
                        }
                        break;
                    default:
                        forEachChild(n, visit);
                        break;
                }

                permittedJumps = savedPermittedJumps;
            }
        }
    }

    export function collectEnclosingScopes(range: RangeToExtract) {
        // 2. collect enclosing scopes
        const scopes: Scope[] = [];
        let current: Node = isArray(range) ? firstOrUndefined(range) : range;
        while (current) {
            if (isFunctionLike(current) || isSourceFile(current) || isModuleBlock(current)) {
                scopes.push(current);
            }
            current = current.parent;
        }
        return scopes;
    }

    export function extractRange(range: RangeToExtract, sourceFile: SourceFile, context: CodeFixContext): ExtractResultForScope[] {
        const scopes = collectEnclosingScopes(range);
        const enclosingTextRange = getEnclosingTextRange(range, sourceFile);
        const { target, usagesPerScope } = collectReadsAndWrites(range, scopes, enclosingTextRange, sourceFile, context);
        context.cancellationToken.throwIfCancellationRequested();
        return usagesPerScope.map((x, i) => extractFunctionInScope(target, scopes[i], x, context))
    }

    export function extractFunctionInScope(node: Node, scope: Scope, usagesInScope: Map<UsageEntry>, context: CodeFixContext): ExtractResultForScope {
        const changeTracker = textChanges.ChangeTracker.fromCodeFixContext(context);
        // TODO: analyze types of usages and introduce type parameters
        // TODO: extract/save information if function has unconditional return/throw/etc..
        // TODO: generate unique function name
        const functionName = "newFunction";
        const parameters: ParameterDeclaration[] = [];
        let writes: UsageEntry[];
        usagesInScope.forEach((value, key) => {
            const paramDecl = createParameter(
                /*decorators*/ undefined,
                /*modifiers*/ undefined,
                /*dotDotDotToken*/ undefined,
                /*name*/ key,
                /*questionToken*/ undefined,
                createKeywordTypeNode(SyntaxKind.AnyKeyword), // TODO: use real type
                );
            parameters.push(paramDecl);
            if (value.usage === Usage.Write) {
                (writes || (writes = [])).push(value);
            }
        });
        const body = transformFunctionBody(node);
        let newFunction: MethodDeclaration | FunctionDeclaration;
        if (isClassLike(scope)) {
            newFunction = createMethod(
                /*decorators*/ undefined,
                /*modifiers*/ undefined,     // TODO: put async if extracted method uses await
                /*asteriskToken*/ undefined, // TODO: put asterisk if extracted method has yield
                functionName,
                /*typeParameters*/ undefined, // TODO: derive type parameters from parameter types
                parameters,
                createKeywordTypeNode(SyntaxKind.AnyKeyword), // TODO: use real type
                body
            );
        }
        else {
            newFunction = createFunctionDeclaration(
                /*decorators*/ undefined,
                /*modifiers*/ undefined,     // TODO: put async if extracted method uses await
                /*asteriskToken*/ undefined, // TODO: put asterisk if extracted method has yield
                functionName,
                /*typeParameters*/ undefined, // TODO: derive type parameters from parameter types
                parameters,
                createKeywordTypeNode(SyntaxKind.AnyKeyword), // TODO: use real type
                body
            );
        }
        return {
            scope,
            changes: changeTracker.getChanges() 
        };

        function transformFunctionBody(n: Node): Block {
            if (isBlock(n) && !writes) {
                return n;
            }
            const statements = isBlock(n) ? n.statements : [isStatement(n) ? n : createStatement(<Expression>n)];
            if (writes) {
                statements.push(createReturn(createObjectLiteral([
                    // TODO: propagate writes back
                ])));
            }
            return createBlock(statements);
        }
    }

    function isModuleBlock(n: Node): n is ModuleBlock {
        return n.kind === SyntaxKind.ModuleBlock;
    }

    function getEnclosingTextRange(range: RangeToExtract, sourceFile: SourceFile): TextRange {
        return isArray(range)
            ? { pos: range[0].getStart(sourceFile), end: lastOrUndefined(range).getEnd() }
            : range;
    }

    const enum Usage {
        // value should be passed to extracted method
        Read  = 1,
        // value should be passed to extracted method and propagated back
        Write = 2
    }

    interface UsageEntry {
        readonly usage: Usage;
        readonly symbol: Symbol;
    }

    function collectReadsAndWrites(
        range: RangeToExtract,
        scopes: Scope[],
        enclosingTextRange: TextRange,
        sourceFile: SourceFile,
        context: CodeFixContext) {

        context.cancellationToken.throwIfCancellationRequested();
        const checker = context.program.getTypeChecker();

        const usagesPerScope = Array<Map<UsageEntry>>(scopes.length);
        const seenUsages = createMap<Usage>();

        let valueUsage = Usage.Read;

        const target = isArray(range) ? createBlock(range) : range;

        forEachChild(target, collectUsages);

        return { target, usagesPerScope }

        function collectUsages(n: Node) {
            if (isAssignmentExpression(n)) {
                const savedValueUsage = valueUsage;
                valueUsage = Usage.Write;
                collectUsages(n.left);
                valueUsage = savedValueUsage;

                collectUsages(n.right);
            }
            else if (isUnaryExpressionWithWrite(n)) {
                const savedValueUsage = valueUsage;
                valueUsage = Usage.Write;
                collectUsages(n.operand);
                valueUsage = savedValueUsage;
            }
            else if (isIdentifier(n)) {
                if (!n.parent) {
                    return;
                }
                if (isQualifiedName(n.parent) && n !== n.parent.left) {
                    return;
                }
                if ((isPropertyAccessExpression(n.parent) || isElementAccessExpression(n.parent)) && n !== n.parent.expression) {
                    return ;
                }
                if (isPartOfTypeNode(n)) {
                    // TODO: check if node is accessible in scope and report an error if it is not
                }
                else {
                    recordUsage(n, valueUsage);
                }
            }
            else {
                forEachChild(n, collectUsages);
            }
        }

        function recordUsage(n: Identifier, usage: Usage) {
            var symbol = checker.getSymbolAtLocation(n);
            if (!symbol) {
                return;
            }
            const symbolId = getSymbolId(symbol).toString();
            const lastUsage = seenUsages.get(symbolId);
            if (lastUsage && lastUsage >= usage) {
                return;
            }

            seenUsages.set(symbolId, usage);
            if (lastUsage) {
                for (const perScope of usagesPerScope) {
                    if (perScope.has(n.text)) {
                        perScope.set(n.text, { usage, symbol });
                    }
                }
                return;
            }
            // find first declaration in this file
            const declInFile = find(symbol.getDeclarations(), d => d.getSourceFile() === sourceFile);
            if (!declInFile) {
                return;
            }
            if (rangeContainsRange(enclosingTextRange, declInFile)) {
                // declaration is located in range to be extracted - do nothing
                return;
            }
            const declContainer = getEnclosingBlockScopeContainer(declInFile);
            for (let i = 0; i < scopes.length; i++) {
                const scope = scopes[i];
                // in order for declaration to be visible in scope block scope container should match or enclose the scope
                if (rangeContainsRange(declContainer, scope)) {
                    const perScope = usagesPerScope[i] || (usagesPerScope[i] = createMap<UsageEntry>());
                    perScope.set(n.text, { usage, symbol });
                }
            }
        }

        function isUnaryExpressionWithWrite(n: Node): n is PrefixUnaryExpression | PostfixUnaryExpression {
            switch (n.kind) {
                case SyntaxKind.PostfixUnaryExpression:
                    return true;
                case SyntaxKind.PrefixUnaryExpression:
                    return (<PrefixUnaryExpression>n).operator === SyntaxKind.PlusPlusToken ||
                        (<PrefixUnaryExpression>n).operator === SyntaxKind.MinusMinusToken;
                default:
                    return false;
            }
        }
    }

    function getParentNodeInSpan(n: Node, file: SourceFile, span: TextSpan): Node {
        while (n) {
            if (!n.parent) {
                return undefined;
            }
            if (isSourceFile(n.parent) || !spanContainsNode(span, n.parent, file)) {
                return n;
            }

            n = n.parent;
        }
    }

    function spanContainsNode(span: TextSpan, node: Node, file: SourceFile): boolean {
        return textSpanContainsPosition(span, node.getStart(file)) &&
            node.getEnd() <= textSpanEnd(span);
    }

    function isBlockLike(n: Node): n is BlockLike {
        switch (n.kind) {
            case SyntaxKind.Block:
            case SyntaxKind.SourceFile:
            case SyntaxKind.ModuleBlock:
            case SyntaxKind.CaseClause:
                return true;
            default:
                return false;
        }
    }
}