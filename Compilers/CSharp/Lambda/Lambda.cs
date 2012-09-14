using System;
using System.Collections;
using System.Collections.Generic;
using System.Linq;
using System.Linq.Expressions;
using System.Text;
using System.Reflection;
using Roslyn.Compilers;
using Roslyn.Compilers.CSharp;

namespace Khaos.Compilers.CSharp
{
    /// <summary>
    /// Build lambda expression trees from snippets of code.
    /// </summary>
    public class Lambda : SyntaxWalker
    {
        /*
         * Thing to think about :-
         * 
         *  - Default Using Statements? Currently Using statements need to be supplied with the method 
         *    which adds to the complexity.
         *    
         */

        #region Default Compilation

        /*
         * TODO :-
         * 
         *  - Create Compilation is currently very naive, this needs to be re-worked
         */

        private static Compilation _DefaultCompilation;

        /// <summary>
        /// Created automatically if null when requested, used when no model is supplied when generating a lambda.
        /// </summary>
        public static Compilation DefaultCompilation
        {
            get
            {
                if (_DefaultCompilation == null)
                    _DefaultCompilation = CreateCompilation();

                return _DefaultCompilation;
            }
            set { _DefaultCompilation = value; }
        }

        /// <summary>
        /// Creates a compilation based on the currently loaded assemblies
        /// </summary>
        /// <returns></returns>
        protected static Compilation CreateCompilation()
        {
            return _DefaultCompilation = Compilation.Create
            (
                "Default Compilation",

                null,
                null,

                from assembly in AppDomain.CurrentDomain.GetAssemblies() where assembly.IsDynamic != true select new AssemblyFileReference(assembly.Location)
            );
        }

        #endregion // </DefaultCompilation>

        /// <summary>
        /// Create lambda from the specified roslyn syntax tree
        /// </summary>
        /// <typeparam name="T">Delegate representing method signature.</typeparam>
        /// <param name="methodSyntax">Method declaration to be converted.</param>
        /// <param name="model">Semantic model based on currently loaded assemblies will be used if not specified (this model is then cached).</param>
        /// <returns>Lambda expression</returns>
        public static T Compile<T>(CompilationUnitSyntax method, SemanticModel model = null)
        {
            if (model == null)
                model = DefaultCompilation.AddSyntaxTrees(method.SyntaxTree).GetSemanticModel(method.SyntaxTree);

            var converter = new Lambda(model);
            converter.Visit(method);
            
            return Expression.Lambda<T>(converter.Body, converter.Parameters.Cast<ParameterExpression>()).Compile();
        }

        /// <summary>
        /// Create lambda from the specified code snippet
        /// </summary>
        /// <typeparam name="T">Delegate representing method signature.</typeparam>
        /// <param name="method">Method declaration in a Roslyn supported language (currently C# or VB)</param>
        /// <param name="model">Semantic model based on currently loaded assemblies will be used if not specified (this model is then cached).</param>
        /// <returns>Lambda expression</returns>
        public static T Compile<T>(string method, SemanticModel model = null)
        {
            var root = (CompilationUnitSyntax)SyntaxTree.ParseCompilationUnit(method).GetRoot();

            if (model == null)
                model = DefaultCompilation.AddSyntaxTrees(root.SyntaxTree).GetSemanticModel(root.SyntaxTree);

            var converter = new Lambda(model);
            converter.Visit(root);

            return Expression.Lambda<T>(converter.Body, converter.Parameters.Cast<ParameterExpression>()).Compile();
        }

        #region SyntaxWalker Implementation

        /// <summary>
        /// Semantic model against which all symbols should be
        /// resolved.
        /// </summary>
        protected SemanticModel Model;

        /// <summary>
        /// 
        /// </summary>
        /// <param name="methodSyntax"></param>
        /// <param name="model"></param>
        public Lambda(SemanticModel model)
        {
            Model = model;

            PushNamedIdentifierScope();
            StartStatementBlock();
        }

        /// <summary>
        /// Method paramaters, set upon entry into VisitMethodDeclaration
        /// </summary>
        protected List<Expression> Parameters = new List<Expression>();

        /// <summary>
        /// Method body, set after VisitMethodDeclaration completes
        /// </summary>
        protected BlockExpression Body;

        /// <summary>
        /// Always the last expression in the method body
        /// </summary>
        protected LabelTarget ReturnTarget;

        /*
         * ISSUE: Need to be able to resolve identifier names all the way from the current scope
         * to the beginning of the stack hierarchy quickly
         */

        /// <summary>
        /// Lookup table of all declared variables accessible from the current scope
        /// </summary>
        public Dictionary<string, Expression> GlobalNamedIdentifierScope = new Dictionary<string, Expression>();

        /// <summary>
        /// List of variables which have been declared in the current scope
        /// </summary>
        public List<string> NamedIdentifierScope = null;

        /// <summary>
        /// Represents the current hierarchy of variable declaration scopes
        /// </summary>
        public Stack<List<string>> NamedIdentifierScopeStack = new Stack<List<string>>();

        /// <summary>
        /// Responsible for settings the Paramaters and Body members.
        /// </summary>
        /// <param name="node"></param>
        public override void VisitMethodDeclaration(MethodDeclarationSyntax node)
        {
            foreach (var parameter in node.ParameterList.Parameters)
                Parameters.Add(DeclareLocalVariable(parameter.Identifier.GetText(), GetType(parameter.Type)));

            ReturnTarget = Expression.Label(GetType(node.ReturnType), "return");

            StartStatementBlock();

            base.Visit(node.Body);

            var localVariables = PopNamedIdentifierScope().Values.Skip(Parameters.Count).OfType<ParameterExpression>().ToArray();

            BlockExpression block = null;
            
            if (localVariables.Length > 0)
            {
                block = Expression.Block
                (
                    localVariables,
                    Expression.Block(EndStatementBlock()),
                    Expression.Label(ReturnTarget, Expression.Default(GetType(node.ReturnType)))
                );
            }
            else
            {
                block = Expression.Block
                (
                    Expression.Block(EndStatementBlock()),
                    Expression.Label(ReturnTarget, Expression.Default(GetType(node.ReturnType)))
                );
            }

            Body = block;
        }

        /// <summary>
        /// 
        /// </summary>
        /// <param name="name"></param>
        /// <param name="param"></param>
        public virtual Expression DeclareLocalVariable(string name, Type type)
        {
            NamedIdentifierScope.Add(name);
            return GlobalNamedIdentifierScope[name] = Expression.Parameter(type, name);
        }

        /// <summary>
        /// 
        /// </summary>
        public void PushNamedIdentifierScope()
        {
            NamedIdentifierScopeStack.Push(NamedIdentifierScope);
            NamedIdentifierScope = new List<string>();
        }

        /// <summary>
        /// 
        /// </summary>
        /// <returns></returns>
        public Dictionary<string, Expression> PopNamedIdentifierScope()
        {
            var toRemove = NamedIdentifierScope;
            NamedIdentifierScope = NamedIdentifierScopeStack.Pop();

            var declarations = new Dictionary<string, Expression>(toRemove.Count);

            foreach (var key in toRemove)
            {
                declarations.Add(key, GlobalNamedIdentifierScope[key]);
                GlobalNamedIdentifierScope.Remove(key);
            }

            return declarations;
        }

        #region Expression Constructs

        /*
         * Region holds all the code associated with constructing the
         * current expression.
         * 
         * TODO:
         *  
         *  - LINQ query support
         * 
         */

        /// <summary>
        /// Represents the current expression being visited in a right to left fashion
        /// </summary>
        public Stack<Expression> ExpressionStack = new Stack<Expression>();

        public override void VisitQueryExpression(QueryExpressionSyntax node)
        {
          
        }

        public override void VisitArrayCreationExpression(ArrayCreationExpressionSyntax node)
        {
            var type = GetType(node.Type.ElementType);

            if (node.Initializer != null)
            {
                /*
                 * Annoyingly this only works for single dimension arrays, an ugly hack could be to add statements
                 * to initialise each index of a multidimensional array...
                 */

                if (node.Type.RankSpecifiers[0].Sizes.Count == 1)
                {
                    var values = new List<Expression>(node.Initializer.Expressions.Count);

                    foreach (var expression in node.Initializer.Expressions)
                    {
                        base.Visit(expression);
                        values.Add(ExpressionStack.Pop());
                    }

                    ExpressionStack.Push(Expression.NewArrayInit(type, values));
                }
                else
                {
                    throw new NotImplementedException("Initialisation of multi-dimensional arrays is not currently possible due to expression tree limitations.");
                }
            }
            else
            {
                if (node.Type.RankSpecifiers.Count > 1)
                {
                    string rankSpecifiers = "";

                    foreach (var specifer in node.Type.RankSpecifiers.Skip(1).Reverse<ArrayRankSpecifierSyntax>())
                        rankSpecifiers += "[" + new String(',', specifer.Sizes.SeparatorCount) + "]";

                    type = MakeArrayType(type, rankSpecifiers);
                }

                var sizes = new List<Expression>();

                foreach (var size in node.Type.RankSpecifiers[0].Sizes)
                {
                    base.Visit(size);
                    sizes.Add(ExpressionStack.Pop());
                }

                ExpressionStack.Push(Expression.NewArrayBounds(type, sizes));
            }
        }

        public override void VisitInitializerExpression(InitializerExpressionSyntax node)
        {
            base.VisitInitializerExpression(node);
        }

        public override void VisitInvocationExpression(InvocationExpressionSyntax node)
        {
            base.VisitInvocationExpression(node);

            var symbolInfo = Model.GetSymbolInfo(node.Expression);

            var containingType = GetType(symbolInfo.Symbol.ContainingType);
            var methodInfo = containingType.GetMethod(symbolInfo.Symbol.MetadataName, (from arg in node.ArgumentList.Arguments select GetType(Model.GetTypeInfo(arg.Expression).Type)).ToArray());

            var argList = new Expression[node.ArgumentList.Arguments.Count];

            for (int i = (argList.Length - 1); i >= 0; i--)
                argList[i] = ExpressionStack.Pop();

            ExpressionStack.Push(Expression.Call(((methodInfo.IsStatic) ? null : ExpressionStack.Pop()), methodInfo, argList));
        }

        public override void VisitObjectCreationExpression(ObjectCreationExpressionSyntax node)
        {
            var type = GetType(node.Type);

            ConstructorInfo constructorInfo = null;
            List<Expression> argList = null;

            if ((node.ArgumentList != null) && (node.ArgumentList.Arguments.Count > 0))
            {
                argList = new List<Expression>();

                foreach (var arg in node.ArgumentList.Arguments)
                {
                    base.Visit(arg.Expression);
                    argList.Add(ExpressionStack.Pop());
                }

                constructorInfo = type.GetConstructor((from argExpression in argList select argExpression.Type).ToArray());
            }
            else
            {
                constructorInfo = type.GetConstructor(new Type[] { });
            }

            var newExp = (argList != null) ? Expression.New(constructorInfo, argList) : Expression.New(constructorInfo);

            if (node.Initializer == null)
            {
                ExpressionStack.Push(Expression.New(constructorInfo, argList));
            }
            else
            {
                switch (node.Initializer.Kind)
                {
                    case SyntaxKind.CollectionInitializerExpression:

                        var addMethodInfo = type.GetMethod("Add");
                        var collInitList = new List<ElementInit>();
                        var argCount = addMethodInfo.GetParameters().Length;

                        foreach (var initExp in node.Initializer.Expressions)
                        {
                            base.Visit(initExp);

                            var initArgs = new Expression[argCount];

                            for (int i = (argCount - 1); i >= 0; i--)
                                initArgs[i] = ExpressionStack.Pop();

                            collInitList.Add(Expression.ElementInit(addMethodInfo, initArgs));
                        }

                        ExpressionStack.Push(Expression.ListInit(newExp, collInitList));

                        break;

                    case SyntaxKind.ObjectInitializerExpression:

                        var memberBinds = new List<MemberBinding>();

                        foreach (BinaryExpressionSyntax bindExp in node.Initializer.Expressions)
                        {
                            base.Visit(bindExp.Right);
                            memberBinds.Add(Expression.Bind(type.GetMember(((IdentifierNameSyntax)bindExp.Left).Identifier.GetText())[0], ExpressionStack.Pop()));
                        }

                        ExpressionStack.Push(Expression.MemberInit(newExp, memberBinds));

                        break;

                    default:
                        throw new NotImplementedException();
                }
            }
        }

        public override void VisitBinaryExpression(BinaryExpressionSyntax node)
        {
            base.VisitBinaryExpression(node);

            Expression rgt = null, lft = null;

            if (ExpressionStack.Count >= 2)
            {
                rgt = ExpressionStack.Pop();
                lft = ExpressionStack.Pop();
            }
            else
            {
                lft = ExpressionStack.Pop();
            }

            switch (node.Kind)
            {
                case SyntaxKind.AddAssignExpression:
                    ExpressionStack.Push(Expression.AddAssign(lft, rgt));
                    break;

                case SyntaxKind.AddExpression:

                    if (lft.Type.TypeHandle.Equals(typeof(string).TypeHandle))
                    {
                        /*
                         * Copy the C# behaviour of mapping String.Concat as the String.op_Addition 
                         * overload for string types.
                         */

                        var objType = typeof(object);

                        lft = Expression.TypeAs(lft, objType);
                        rgt = Expression.TypeAs(rgt, objType);

                        ExpressionStack.Push(Expression.Add(lft, rgt, lft.Type.GetMethod("Concat", new[] { objType, objType })));
                    }
                    else
                    {
                        ExpressionStack.Push(Expression.Add(lft, rgt));
                    }

                    break;

                case SyntaxKind.AsExpression:
                    ExpressionStack.Push(Expression.Convert(lft, GetType(Model.GetTypeInfo(node.Right).Type)));
                    break;

                case SyntaxKind.AssignExpression:
                    ExpressionStack.Push(Expression.Assign(lft, rgt));
                    break;

                case SyntaxKind.AndAssignExpression:
                    ExpressionStack.Push(Expression.AndAssign(lft, rgt));
                    break;

                case SyntaxKind.BitwiseAndExpression:
                    ExpressionStack.Push(Expression.And(lft, rgt));
                    break;

                case SyntaxKind.BitwiseOrExpression:
                    ExpressionStack.Push(Expression.Or(lft, rgt));
                    break;

                case SyntaxKind.CoalesceExpression:
                    ExpressionStack.Push(Expression.Coalesce(lft, rgt));
                    break;

                case SyntaxKind.DivideAssignExpression:
                    ExpressionStack.Push(Expression.DivideAssign(lft, rgt));
                    break;

                case SyntaxKind.DivideExpression:
                    ExpressionStack.Push(Expression.Divide(lft, rgt));
                    break;

                case SyntaxKind.EqualsExpression:
                    ExpressionStack.Push(Expression.Equal(lft, rgt));
                    break;

                case SyntaxKind.ExclusiveOrAssignExpression:
                    ExpressionStack.Push(Expression.ExclusiveOrAssign(lft, rgt));
                    break;

                case SyntaxKind.ExclusiveOrExpression:
                    ExpressionStack.Push(Expression.ExclusiveOr(lft, rgt));
                    break;

                case SyntaxKind.GreaterThanExpression:
                    ExpressionStack.Push(Expression.GreaterThan(lft, rgt));
                    break;

                case SyntaxKind.GreaterThanOrEqualExpression:
                    ExpressionStack.Push(Expression.GreaterThanOrEqual(lft, rgt));
                    break;

                case SyntaxKind.IsExpression:
                    ExpressionStack.Push(Expression.TypeIs(lft, GetType(Model.GetTypeInfo(node.Right).Type)));
                    break;

                case SyntaxKind.LeftShiftAssignExpression:
                    ExpressionStack.Push(Expression.LeftShiftAssign(lft, rgt));
                    break;

                case SyntaxKind.LeftShiftExpression:
                    ExpressionStack.Push(Expression.LeftShift(lft, rgt));
                    break;

                case SyntaxKind.LessThanExpression:
                    ExpressionStack.Push(Expression.LessThan(lft, rgt));
                    break;

                case SyntaxKind.LessThanOrEqualExpression:
                    ExpressionStack.Push(Expression.LessThanOrEqual(lft, rgt));
                    break;

                case SyntaxKind.LogicalAndExpression:
                    ExpressionStack.Push(Expression.AndAlso(lft, rgt));
                    break;

                case SyntaxKind.LogicalOrExpression:
                    ExpressionStack.Push(Expression.OrElse(lft, rgt));
                    break;

                case SyntaxKind.ModuloAssignExpression:
                    ExpressionStack.Push(Expression.ModuloAssign(lft, rgt));
                    break;

                case SyntaxKind.ModuloExpression:
                    ExpressionStack.Push(Expression.Modulo(lft, rgt));
                    break;

                case SyntaxKind.MultiplyAssignExpression:
                    ExpressionStack.Push(Expression.MultiplyAssign(lft, rgt));
                    break;

                case SyntaxKind.MultiplyExpression:
                    ExpressionStack.Push(Expression.Multiply(lft, rgt));
                    break;

                case SyntaxKind.NotEqualsExpression:
                    ExpressionStack.Push(Expression.NotEqual(lft, rgt));
                    break;

                case SyntaxKind.OrAssignExpression:
                    ExpressionStack.Push(Expression.OrAssign(lft, rgt));
                    break;

                case SyntaxKind.RightShiftAssignExpression:
                    ExpressionStack.Push(Expression.RightShiftAssign(lft, rgt));
                    break;

                case SyntaxKind.RightShiftExpression:
                    ExpressionStack.Push(Expression.RightShift(lft, rgt));
                    break;

                case SyntaxKind.SubtractAssignExpression:
                    ExpressionStack.Push(Expression.SubtractAssign(lft, rgt));
                    break;

                case SyntaxKind.SubtractExpression:
                    ExpressionStack.Push(Expression.Subtract(lft, rgt));
                    break;

                default:
                    throw new NotImplementedException();
            }
        }

        public override void VisitLiteralExpression(LiteralExpressionSyntax node)
        {
            ExpressionStack.Push(Expression.Constant(node.Token.Value));
        }

        public override void VisitIdentifierName(IdentifierNameSyntax node)
        {
            Expression identifier;

            if (GlobalNamedIdentifierScope.TryGetValue(node.GetText(), out identifier))
                ExpressionStack.Push(identifier);
        }

        public override void VisitMemberAccessExpression(MemberAccessExpressionSyntax node)
        {
            base.VisitMemberAccessExpression(node);
            
            var symbol = Model.GetSymbolInfo(node.Name).Symbol;
            var containingType = GetType(symbol.ContainingType);

            switch (symbol.Kind)
            {
                case SymbolKind.Property:
                    ExpressionStack.Push(Expression.MakeMemberAccess((symbol.IsStatic) ? null : ExpressionStack.Pop(), containingType.GetMember(symbol.MetadataName).First()));
                    break;

                case SymbolKind.Field:
                    ExpressionStack.Push(Expression.MakeMemberAccess((symbol.IsStatic) ? null : ExpressionStack.Pop(), containingType.GetMember(symbol.MetadataName).First()));
                    break;

                case SymbolKind.Method:
                    break;

                default:
                    throw new NotImplementedException();
            }
        }

        public override void VisitElementAccessExpression(ElementAccessExpressionSyntax node)
        {
            base.Visit(node.Expression);
            
            var expression = ExpressionStack.Pop();
            var arguments = new List<Expression>(node.ArgumentList.Arguments.Count);

            foreach (var argument in node.ArgumentList.Arguments)
            {
                base.Visit(argument);
                arguments.Add(ExpressionStack.Pop());
            }

            if (expression.Type.IsArray)
            {
                ExpressionStack.Push(Expression.ArrayAccess(expression, arguments));
            }
            else
            {
                ExpressionStack.Push(Expression.Property(expression, expression.Type.GetProperty("Item"), arguments.ToArray()));
            }
        }

        public override void VisitPostfixUnaryExpression(PostfixUnaryExpressionSyntax node)
        {
            base.VisitPostfixUnaryExpression(node);

            switch (node.Kind)
            {
                case SyntaxKind.PostIncrementExpression:
                    ExpressionStack.Push(Expression.PostIncrementAssign(ExpressionStack.Pop()));
                    break;

                case SyntaxKind.PostDecrementExpression:
                    ExpressionStack.Push(Expression.PostDecrementAssign(ExpressionStack.Pop()));
                    break;

                default:
                    throw new NotImplementedException();
            }
        }

        public override void VisitPrefixUnaryExpression(PrefixUnaryExpressionSyntax node)
        {
            base.VisitPrefixUnaryExpression(node);

            switch (node.Kind)
            {
                case SyntaxKind.PreIncrementExpression:
                    ExpressionStack.Push(Expression.PreIncrementAssign(ExpressionStack.Pop()));
                    break;

                case SyntaxKind.PreDecrementExpression:
                    ExpressionStack.Push(Expression.PreDecrementAssign(ExpressionStack.Pop()));
                    break;

                case SyntaxKind.BitwiseNotExpression:
                    ExpressionStack.Push(Expression.Not(ExpressionStack.Pop()));
                    break;

                case SyntaxKind.LogicalNotExpression:
                    ExpressionStack.Push(Expression.Negate(ExpressionStack.Pop()));
                    break;

                case SyntaxKind.PlusExpression:
                    ExpressionStack.Push(Expression.UnaryPlus(ExpressionStack.Pop()));
                    break;

                case SyntaxKind.NegateExpression:
                    ExpressionStack.Push(Expression.Negate(ExpressionStack.Pop()));
                    break;

                default:
                    throw new NotImplementedException();
            }
        }

        public override void VisitTypeOfExpression(TypeOfExpressionSyntax node)
        {
            ExpressionStack.Push(Expression.Constant(GetType(node.Type)));
        }

        public override void VisitCastExpression(CastExpressionSyntax node)
        {
            base.VisitCastExpression(node);
            ExpressionStack.Push(Expression.Convert(ExpressionStack.Pop(), GetType(node.Type)));
        }

        public override void VisitConditionalExpression(ConditionalExpressionSyntax node)
        {
            base.VisitConditionalExpression(node);

            var ifFalse = ExpressionStack.Pop();
            var ifTrue  = ExpressionStack.Pop();
            var test    = ExpressionStack.Pop();

            ExpressionStack.Push(Expression.Condition(test, ifTrue, ifFalse));
        }

        public override void VisitCheckedExpression(CheckedExpressionSyntax node)
        {
            switch (node.Kind)
            {
                case SyntaxKind.CheckedExpression:
                    throw new NotImplementedException("checked expressions not supporterd");

                case SyntaxKind.UncheckedExpression:
                    throw new NotImplementedException("unchecked expressions not supported");

                default:
                    throw new NotImplementedException();
            }
        }

  

        #endregion // </Expression Constructs>

        #region Statement Constructs

        /*
         * Region holds all the code associated with building the
         * statement blocks.
         * 
         * TODO :-
         * 
         *  - Foreach Statement
         *  - Try Catch Finally Statement
         *  
         */

        /// <summary>
        /// Represents a list of statements within the scope of the current statement block
        /// </summary>
        public List<Expression> CurrentStatementBlock;

        /// <summary>
        /// Represents the current hierarchy of statement blocks
        /// </summary>
        /// <remarks>
        /// Each time a statement block is popped off of the stack it should typically
        /// be converted to an BlockExpression and added to the CurrentStatementBlock list
        /// </remarks>
        protected Stack<List<Expression>> StatementBlockStack = new Stack<List<Expression>>();

        /// <summary>
        /// Represents the jump point for breaking the current loop or switch statement
        /// </summary>
        protected LabelTarget BreakTarget = null;

        /// <summary>
        /// Represents the current hierarchy of break target points of nested
        /// loops and switch statements.
        /// </summary>
        protected Stack<LabelTarget> BreakTargetStack = new Stack<LabelTarget>();

        /// <summary>
        /// Represents the jump point for skipping to the next iteration
        /// </summary>
        protected LabelTarget ContinueTarget = null;

        /// <summary>
        /// Represents the current hierarchy of continue target points of nested
        /// loops.
        /// </summary>
        protected Stack<LabelTarget> ContinueTargetStack = new Stack<LabelTarget>();

        public override void VisitSwitchStatement(SwitchStatementSyntax node)
        {
            PushBreakTarget();

            Expression defaultCase = null;
            List<SwitchCase> switchCases = new List<SwitchCase>(node.Sections.Count);

            foreach (var section in node.Sections)
            {
                StartStatementBlock();
                foreach(var statement in section.Statements) base.Visit(statement);
                var statementBlock = EndStatementBlock();

                if (section.Labels[0].Kind == SyntaxKind.CaseSwitchLabel)
                {
                    var labels = new List<Expression>(section.Labels.Count);

                    foreach (var label in section.Labels)
                    {
                        base.Visit(label.Value);
                        labels.Add(ExpressionStack.Pop());
                    }

                    switchCases.Add(Expression.SwitchCase(Expression.Block(statementBlock), labels));
                }
                else
                {
                    defaultCase = Expression.Block(statementBlock);
                }
            }

            base.Visit(node.Expression);
            SwitchExpression switchStatement = null;

            if (defaultCase == null)
            {
                switchStatement = Expression.Switch(ExpressionStack.Pop(), switchCases.ToArray());
            }
            else
            {
                switchStatement = Expression.Switch(ExpressionStack.Pop(), defaultCase, switchCases.ToArray());
            }

            CurrentStatementBlock.Add(Expression.Block(switchStatement, Expression.Label(PopBreakTarget())));
        }

        public override void VisitBreakStatement(BreakStatementSyntax node)
        {
            CurrentStatementBlock.Add(Expression.Break(BreakTarget));
        }

        public override void VisitContinueStatement(ContinueStatementSyntax node)
        {
            CurrentStatementBlock.Add(Expression.Continue(ContinueTarget));
        }

        public override void VisitIfStatement(IfStatementSyntax node)
        {
            base.Visit(node.Condition);

            PushNamedIdentifierScope();
            StartStatementBlock();

            base.Visit(node.Statement);

            var statementBlock = EndStatementBlock();
            var statementParams = PopNamedIdentifierScope().Values.Cast<ParameterExpression>();
            
            if (node.Else != null)
            {
                PushNamedIdentifierScope();
                StartStatementBlock();

                base.Visit(node.Else);

                var elseStatementBlock = EndStatementBlock();
                var elseStatementParams = PopNamedIdentifierScope().Values.Cast<ParameterExpression>();

                CurrentStatementBlock.Add(Expression.IfThenElse(ExpressionStack.Pop(), Expression.Block(statementParams, statementBlock), Expression.Block(elseStatementParams, elseStatementBlock)));
            }
            else
            {
                CurrentStatementBlock.Add(Expression.IfThen(ExpressionStack.Pop(), Expression.Block(statementParams, statementBlock)));
            }
        }

        public override void VisitForStatement(ForStatementSyntax node)
        {
            PushNamedIdentifierScope();

            PushBreakTarget();
            PushContinueTarget();

            /*
             * Prepare all the individual parts that make up
             * the for loop
             */

            List<Expression> declarations = null;

            if (node.Declaration != null)
            {
                declarations = new List<Expression>();
                var type = GetType(node.Declaration.Type);

                foreach (var variable in node.Declaration.Variables)
                {
                    base.Visit(variable.Initializer);
                    declarations.Add(Expression.Assign(DeclareLocalVariable(variable.Identifier.ValueText, type), ExpressionStack.Pop()));
                }
            }

            Expression test = null;

            if (node.Condition != null)
            {
                base.Visit(node.Condition);
                test = ExpressionStack.Pop();
            }

            List<Expression> incrementors = null;

            if (node.Incrementors.Count > 0)
            {
                incrementors = new List<Expression>();

                foreach (var incrementor in node.Incrementors)
                {
                    base.Visit(incrementor);
                    incrementors.Add(ExpressionStack.Pop());
                }
            }

            List<Expression> initializers = null;

            if (node.Initializers.Count > 0)
            {
                initializers = new List<Expression>(node.Initializers.Count);

                foreach (var initializer in node.Initializers)
                {
                    base.Visit(initializer);
                    initializers.Add(ExpressionStack.Pop());
                }
            }

            StartStatementBlock();
            base.Visit(node.Statement);
            var statements = EndStatementBlock();

            /*
             * Construct for loop
             */

            Expression innerBody;

            if (incrementors != null)
            {
                innerBody = Expression.Block
                (
                    Expression.Block(statements),
                    Expression.Block(incrementors)
                );
            }
            else
            {
                innerBody = Expression.Block(statements);
            }

            Expression loopBody;

            if (test != null)
            {
                loopBody = Expression.IfThenElse
                (
                    test,

                    innerBody,
                    Expression.Break(BreakTarget)
                );
            }
            else
            {
                loopBody = innerBody;
            }

            var forLoop = new List<Expression>();

            if (declarations != null)
                forLoop.AddRange(declarations);

            if (initializers != null)
                forLoop.AddRange(initializers);

            forLoop.Add(Expression.Loop
            (
                loopBody,

                PopBreakTarget(),
                PopContinueTarget()
            ));

            CurrentStatementBlock.Add(Expression.Block
            (
                PopNamedIdentifierScope().Values.OfType<ParameterExpression>(),
                forLoop
            ));
        }

        public override void VisitForEachStatement(ForEachStatementSyntax node)
        {
            PushNamedIdentifierScope();

            PushBreakTarget();
            PushContinueTarget();
            
            base.Visit(node.Expression);
            var expression = ExpressionStack.Pop();

            Expression
                identifier    = null,
                enumerator    = null,
                getEnumerator = null,
                moveNext      = null,
                current       = null;

            if (node.Type.IsVar == false)
                identifier = DeclareLocalVariable(node.Identifier.GetText(), GetType(node.Type));

            if (expression.Type.IsArray)
            {
                var elementType = expression.Type.GetElementType();

                var enumerableType = typeof(IEnumerable<>).MakeGenericType(elementType);
                var enumeratorType = typeof(IEnumerator<>).MakeGenericType(elementType);

                if (identifier == null)
                    identifier = DeclareLocalVariable(node.Identifier.GetText(), elementType);

                enumerator    = Expression.Variable(enumeratorType, "enumerator");
                getEnumerator = Expression.Assign(enumerator, Expression.Call(Expression.Convert(expression, enumerableType), enumerableType.GetMethod("GetEnumerator")));
                moveNext      = Expression.Call(enumerator, typeof(IEnumerator).GetMethod("MoveNext"));
                current       = Expression.Assign(identifier, Expression.Property(enumerator, "Current"));
            }
            else
            {
                var miGetEnumerator = expression.Type.GetMethod("GetEnumerator");

                if (identifier == null)
                    identifier = DeclareLocalVariable(node.Identifier.GetText(), miGetEnumerator.ReturnType.GetProperty("Current").PropertyType);

                enumerator    = Expression.Variable(miGetEnumerator.ReturnType, "enumerator");
                getEnumerator = Expression.Assign(enumerator, Expression.Call(expression, miGetEnumerator));
                moveNext      = Expression.Call(enumerator, enumerator.Type.GetMethod("MoveNext"));
                current       = Expression.Assign(identifier, Expression.Property(enumerator, "Current"));
            }

            StartStatementBlock();
            base.Visit(node.Statement);
            var statements = EndStatementBlock();

            var blockParams = new List<ParameterExpression>();
            blockParams.AddRange(PopNamedIdentifierScope().Values.Cast<ParameterExpression>());
            blockParams.Add((ParameterExpression)enumerator);

            CurrentStatementBlock.Add(Expression.Block
            (
                blockParams,
                getEnumerator,

                Expression.Loop
                (
                    Expression.IfThenElse
                    (
                        Expression.NotEqual(moveNext, Expression.Constant(false)),

                        Expression.Block
                        (
                            Expression.Block(current),
                            Expression.Block(statements)
                        ),

                        Expression.Break(BreakTarget)
                    ),

                    PopBreakTarget(),
                    PopContinueTarget()
                )
            ));
        }

        public override void VisitWhileStatement(WhileStatementSyntax node)
        {
            PushNamedIdentifierScope();

            PushBreakTarget();
            PushContinueTarget();
            
            StartStatementBlock();
            base.Visit(node.Statement);
            var statements = EndStatementBlock();

            base.Visit(node.Condition);

            CurrentStatementBlock.Add
            (
                Expression.Loop
                (
                    Expression.IfThenElse
                    (
                        ExpressionStack.Pop(),

                        Expression.Block(PopNamedIdentifierScope().Values.Cast<ParameterExpression>(), statements),
                        Expression.Break(BreakTarget)
                    ),

                    PopBreakTarget(), 
                    PopContinueTarget()
                )
            );
        }

        public override void VisitDoStatement(DoStatementSyntax node)
        {
            PushNamedIdentifierScope();

            PushBreakTarget();
            PushContinueTarget();

            StartStatementBlock();
            base.Visit(node.Statement);
            var statements = EndStatementBlock();

            base.Visit(node.Condition);

            CurrentStatementBlock.Add(Expression.Loop
            (
                Expression.Block
                (
                    Expression.Block(PopNamedIdentifierScope().Values.Cast<ParameterExpression>(), statements),

                    Expression.IfThenElse
                    (
                        ExpressionStack.Pop(),

                        Expression.Continue(ContinueTarget),
                        Expression.Break(BreakTarget)
                    )
                ),

                PopBreakTarget(),
                PopContinueTarget()
            ));
        }

        public override void VisitLocalDeclarationStatement(LocalDeclarationStatementSyntax node)
        {
            Type type = null;

            if (node.Declaration.Type.IsVar == false)
                type = GetType(node.Declaration.Type);

            foreach (var variable in node.Declaration.Variables)
            {
                if (variable.Initializer != null)
                {
                    base.Visit(variable.Initializer);

                    var value = ExpressionStack.Pop();

                    if ((type != null) && (type.Equals(value.Type) != true))
                        value = Expression.Convert(value, type);
                    
                    CurrentStatementBlock.Add(Expression.Assign(DeclareLocalVariable(variable.Identifier.GetText(), (type != null) ? type : value.Type), value));
                }
                else
                {
                    DeclareLocalVariable(variable.Identifier.GetText(), type);
                }
            }
        }

        public override void VisitExpressionStatement(ExpressionStatementSyntax node)
        {
            base.VisitExpressionStatement(node);

            CurrentStatementBlock.Add(ExpressionStack.Pop());
        }

        public override void VisitReturnStatement(ReturnStatementSyntax node)
        {
            base.VisitReturnStatement(node);

            if (ExpressionStack.Count == 0)
            {
                CurrentStatementBlock.Add(Expression.Return(ReturnTarget));
            }
            else
            {
                CurrentStatementBlock.Add(Expression.Return(ReturnTarget, ExpressionStack.Pop()));
            }
        }

        public override void VisitThrowStatement(ThrowStatementSyntax node)
        {
            base.VisitThrowStatement(node);

            CurrentStatementBlock.Add(Expression.Throw(ExpressionStack.Pop()));
        }

        public override void VisitGotoStatement(GotoStatementSyntax node)
        {
            base.VisitGotoStatement(node);
            CurrentStatementBlock.Add(Expression.Goto(Expression.Label(((IdentifierNameSyntax)node.Expression).PlainName)));
        }

        public override void VisitLabeledStatement(LabeledStatementSyntax node)
        {
            StartStatementBlock();
            base.VisitLabeledStatement(node);
            var labelStatement = EndStatementBlock();

            CurrentStatementBlock.Add(Expression.Label(Expression.Label(node.Identifier.ValueText), labelStatement[0]));
        }

        public override void VisitTryStatement(TryStatementSyntax node)
        {
            PushNamedIdentifierScope();

            StartStatementBlock();
            base.Visit(node.Block);

            var tryBlock = Expression.Block(PopNamedIdentifierScope().Values.Cast<ParameterExpression>(), EndStatementBlock());

            var catchBlocks = new List<CatchBlock>();

            foreach (var catchClause in node.Catches)
            {
                ParameterExpression declaration = null;

                PushNamedIdentifierScope();

                if (catchClause.Declaration != null)
                    declaration = (ParameterExpression)DeclareLocalVariable(catchClause.Declaration.Identifier.GetText(), GetType(catchClause.Declaration.Type));

                StartStatementBlock();
                base.Visit(catchClause.Block);
                var catchBlock = Expression.Block(PopNamedIdentifierScope().Values.Cast<ParameterExpression>(), EndStatementBlock());

                if (declaration != null)
                {
                    catchBlocks.Add(Expression.Catch(declaration, catchBlock));
                }
                else
                {
                    catchBlocks.Add(Expression.Catch(typeof(Exception), catchBlock));
                }
            }

            Expression finallyBlock = null;

            if (node.Finally != null)
            {
                PushNamedIdentifierScope();

                StartStatementBlock();
                base.Visit(node.Finally.Block);

                finallyBlock = Expression.Block(PopNamedIdentifierScope().Values.Cast<ParameterExpression>(), EndStatementBlock());
            }

            if ((catchBlocks.Count > 0) && (finallyBlock == null))
                CurrentStatementBlock.Add(Expression.TryCatch(tryBlock, catchBlocks.ToArray()));

            if ((catchBlocks.Count > 0) && (finallyBlock != null))
                CurrentStatementBlock.Add(Expression.TryCatchFinally(tryBlock, finallyBlock, catchBlocks.ToArray()));

            if ((catchBlocks.Count == 0) && (finallyBlock != null))
                CurrentStatementBlock.Add(Expression.TryFinally(tryBlock, finallyBlock));
        }

        /// <summary>
        /// Marks a new break target whilst saving the current target.
        /// </summary>
        protected void PushBreakTarget()
        {
            BreakTargetStack.Push(BreakTarget);
            BreakTarget = Expression.Label("break");
        }

        /// <summary>
        /// Discards the current target whilst re-instating the previous target.
        /// </summary>
        protected LabelTarget PopBreakTarget()
        {
            var ret = BreakTarget;
            BreakTarget = BreakTargetStack.Pop();

            return ret;
        }

        /// <summary>
        /// Marks a new continue target whilst saving the current target.
        /// </summary>
        protected void PushContinueTarget()
        {
            ContinueTargetStack.Push(ContinueTarget);
            ContinueTarget = Expression.Label("continue");
        }

        /// <summary>
        /// Discards the current target whilst re-instating the previous target.
        /// </summary>
        protected LabelTarget PopContinueTarget()
        {
            var ret = ContinueTarget;
            ContinueTarget = ContinueTargetStack.Pop();

            return ret;
        }

        /// <summary>
        /// Starts a nested statement block in a new scope
        /// </summary>
        protected void StartStatementBlock()
        {
            StatementBlockStack.Push(CurrentStatementBlock = new List<Expression>());
        }

        /// <summary>
        /// Ends the current scope statement block scope whilst re-initialising 
        /// the parent scope.
        /// </summary>
        /// <returns>List of statements popped off of the stack</returns>
        protected List<Expression> EndStatementBlock()
        {
            var ret = StatementBlockStack.Pop();

            if (StatementBlockStack.Count > 0)
            {
                CurrentStatementBlock = StatementBlockStack.Peek();
            }
            else
            {
                CurrentStatementBlock = new List<Expression>();
            }

            return ret;
        }

        #endregion // </Statement Constructs>

        #region Helper Methods

        /*
         * Various methods used throughout to reduce code duplication.
         */

        #region GetType

        /*
         * To Think About :-
         * 
         *  - Is this really the best way to get the System.Type of the symbol were interested in? seems a bit hackish...
         *  - Is it worth caching the results? need to run more tests and see where the biggest bottle necks are
         */

        protected Type MakeArrayType(Type type, string rankSpecifiers)
        {
            return Type.GetType(type.FullName + rankSpecifiers + ", " + type.Assembly);
        }

        protected Type GetType(TypeSyntax typeSyntax)
        {
            return Type.GetType(BuildGetTypeString(typeSyntax), true);
        }

        protected Type GetType(TypeSymbol typeSymbol)
        {
            return Type.GetType(BuildGetTypeString(typeSymbol), true);
        }

        protected string BuildGetTypeString(TypeSyntax typeSyntax, string arrayRankSpecifiers = "")
        {
            var typeArguments = "";

            switch (typeSyntax.Kind)
            {
                case SyntaxKind.ArrayType:

                    var arrayTypeSyntax = (ArrayTypeSyntax)typeSyntax;
                    var rankSpecifiers = "";

                    foreach (var specifier in arrayTypeSyntax.RankSpecifiers.Reverse<ArrayRankSpecifierSyntax>())
                        rankSpecifiers += "[" + new String(',', specifier.Sizes.SeparatorCount) + "]";

                    return BuildGetTypeString(arrayTypeSyntax.ElementType, rankSpecifiers);

                case SyntaxKind.GenericName:

                    var genericNameSyntax = (GenericNameSyntax)typeSyntax;
                    var typeArgsList = new List<string>();

                    foreach (var typeArg in genericNameSyntax.TypeArgumentList.Arguments)
                        typeArgsList.Add("[" + BuildGetTypeString(typeArg) + "]");

                    typeArguments = "[" + String.Join(",", typeArgsList) + "]";

                    break;
            }

        

            var typeSymbol = Model.GetTypeInfo(typeSyntax).Type;

            //var typeSymbol = Model.GetSpeculativeTypeInfo(152, typeSyntax, Roslyn.Compilers.Common.SpeculativeBindingOption.BindAsTypeOrNamespace).Type;

            return typeSymbol.ContainingNamespace.ToString() + "." + typeSymbol.MetadataName + typeArguments + arrayRankSpecifiers + ", " + typeSymbol.ContainingAssembly.ToString();
        }

        protected string BuildGetTypeString(TypeSymbol typeSymbol, string arrayRank = "")
        {
            var typeArguments = "";

            switch (typeSymbol.Kind)
            {
                case SymbolKind.ArrayType:

                    return BuildGetTypeString(((ArrayTypeSymbol)typeSymbol).ElementType, "[" + new String(',', ((ArrayTypeSymbol)typeSymbol).Rank) + "]");

                case SymbolKind.NamedType:

                    var namedTypeSymbol = (NamedTypeSymbol)typeSymbol;

                    if (namedTypeSymbol.IsGenericType)
                    {
                        var typeArgsList = new List<string>();

                        foreach (var typeArg in namedTypeSymbol.TypeArguments)
                            typeArgsList.Add("[" + BuildGetTypeString(typeArg) + "]");

                        typeArguments = "[" + String.Join(",", typeArgsList) + "]";
                    }

                    break;
            }

            return typeSymbol.ContainingNamespace.ToString() + "." + typeSymbol.MetadataName + typeArguments + arrayRank + ", " + typeSymbol.ContainingAssembly.ToString();
        }



        #endregion // </GetType>

        #endregion // </Helper Methods>

        #endregion // </SyntaxWalker Implementation>
    }
}
