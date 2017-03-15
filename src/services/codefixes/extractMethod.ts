/* @internal */
namespace ts.codefix.extractMethod {
    export type RangeToExtract = Expression | Statement[];
    export type Scope = FunctionLikeDeclaration | SourceFile;
    export interface ExtractResultForScope {
        readonly scope: Scope;
        readonly changes: TextChange[];
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
            if (isFunctionLike(current) || isSourceFile(current)) {
                scopes.push(current);
            }
            current = current.parent;
        }
        return scopes;
    }

    export function extractRange(range: RangeToExtract, sourceFile: SourceFile, program: Program, cancellationToken: CancellationToken): ExtractResultForScope[] {
        const scopes = collectEnclosingScopes(range);
        const enclosingTextRange = getEnclosingTextRange(range, sourceFile);

        const { target, readsForScopes, writesForScopes } = collectReadsAndWrites(range, scopes, program.getTypeChecker(), enclosingTextRange);
    }

    function isTargetOfRead(n: Identifier) {
        if (!n.parent) {
            return false;
        }
        if (isQualifiedName(n.parent)) {
            return n === n.parent.left;
        }
        else if (isPropertyAccessExpression(n.parent)) {
            return n === n.parent.expression;
        }
        return true;
    }

    function getEnclosingTextRange(range: RangeToExtract, sourceFile: SourceFile): TextRange {
        return isArray(range)
            ? { pos: range[0].getStart(sourceFile), end: lastOrUndefined(range).getEnd() }
            : range;
    }    

    function collectReadsAndWrites(range: RangeToExtract, scopes: Scope[], checker: TypeChecker, enclosingTextRange: TextRange) {
        const readsForScopes = Array<Map<void>>(scopes.length);
        const writesForScopes = Array<Map<void>>(scopes.length);

        const target = isArray(range) ? createBlock(range) : range;

        forEachChild(target, collectUsages);

        return { target, readsForScopes, writesForScopes }


        function collectUsages(n: Node) {
            if (isAssignmentExpression(n)) {
                visitLHS(n.left);
                collectUsages(n.right);
            }
            else if (isUnaryExpressionWithWrite(n)) {
                visitLHS(n.operand);
            }
            else if (isIdentifier(n) && isTargetOfRead(n)) {
                recordRead(n);
            }
            else {
                forEachChild(n, collectUsages);
            }
        }

        function visitLHS(n: Node) {

        }

        function recordRead(n: Identifier) {
            var symbol = checker.getSymbolAtLocation(n);
            if (!symbol) {
                return;
            }
            for (let i = 0; i < scopes.length; i++) {
                let reads = readsForScopes[i];
                if (reads && reads.has(n.text)) {
                    continue;
                }
                const scope = scopes[i];
                if (symbol.valueDeclaration && !rangeContainsRange(scope, symbol.valueDeclaration)) {
                    if (!reads) {
                        readsForScopes[i] = reads = createMap<void>();
                    }
                    reads.set(n.text, undefined);
                }
            }
        }

        function recordWrite(n: Identifier) {
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
}