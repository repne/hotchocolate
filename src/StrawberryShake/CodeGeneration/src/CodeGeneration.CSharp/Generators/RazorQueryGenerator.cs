using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using StrawberryShake.CodeGeneration.CSharp.Extensions;
using StrawberryShake.CodeGeneration.Descriptors.Operations;
using StrawberryShake.CodeGeneration.Descriptors.TypeDescriptors;
using StrawberryShake.CodeGeneration.Extensions;
using static Microsoft.CodeAnalysis.CSharp.SyntaxFactory;
using static StrawberryShake.CodeGeneration.Utilities.NameUtils;

namespace StrawberryShake.CodeGeneration.CSharp.Generators;

public class RazorQueryGenerator : CSharpSyntaxGenerator<OperationDescriptor>
{
    protected override bool CanHandle(
        OperationDescriptor descriptor,
        CSharpSyntaxGeneratorSettings settings) =>
        settings.RazorComponents && descriptor is QueryOperationDescriptor;

    protected override CSharpSyntaxGeneratorResult Generate(
        OperationDescriptor descriptor,
        CSharpSyntaxGeneratorSettings settings)
    {
        var componentName = $"Use{descriptor.Name}";
        var resultType = descriptor.ResultTypeReference.GetRuntimeType().ToString();

        var modifier = settings.AccessModifier == AccessModifier.Public
            ? SyntaxKind.PublicKeyword
            : SyntaxKind.InternalKeyword;

        var classDeclaration =
            ClassDeclaration(componentName)
                .AddImplements(TypeNames.UseQuery.WithGeneric(resultType))
                .AddModifiers(
                    Token(modifier),
                    Token(SyntaxKind.PartialKeyword))
                .AddGeneratedAttribute()
                .AddMembers(CreateOperationProperty(descriptor.RuntimeType.ToString()));

        foreach (var argument in descriptor.Arguments)
        {
            classDeclaration = classDeclaration.AddMembers(
                CreateArgumentProperty(argument));
        }

#if NET8_0
        classDeclaration = classDeclaration.AddMembers(
            CreateLifecycleMethodMethod("OnInitialized", descriptor.Arguments));
#else
        classDeclaration = classDeclaration.AddMembers(
            CreateOnInitializedAsyncMethod(descriptor));
#endif

        classDeclaration = classDeclaration.AddMembers(
            CreateLifecycleMethodMethod("OnParametersSet", descriptor.Arguments));

        return new CSharpSyntaxGeneratorResult(
            componentName,
            Components,
            $"{descriptor.RuntimeType.NamespaceWithoutGlobal}.{Components}",
            classDeclaration,
            isRazorComponent: true);
    }

    private PropertyDeclarationSyntax CreateOperationProperty(string typeName) =>
        PropertyDeclaration(ParseTypeName(typeName), "Operation")
            .WithAttributeLists(
                SingletonList(
                    AttributeList(
                        SingletonSeparatedList(
                            Attribute(
                                IdentifierName(TypeNames.InjectAttribute))))))
            .AddModifiers(Token(SyntaxKind.InternalKeyword))
            .WithGetterAndSetter()
            .WithSuppressNullableWarningExpression();

    private PropertyDeclarationSyntax CreateArgumentProperty(PropertyDescriptor property)
    {
        var propertySyntax =
            PropertyDeclaration(property.Type.ToTypeSyntax(), GetPropertyName(property.Name))
                .WithAttributeLists(
                    SingletonList(
                        AttributeList(
                            SingletonSeparatedList(
                                Attribute(
                                    IdentifierName(TypeNames.ParameterAttribute))))))
                .AddModifiers(Token(SyntaxKind.PublicKeyword))
                .WithGetterAndSetter();

        if (property.Type.IsNonNull())
        {
            propertySyntax = propertySyntax.WithSuppressNullableWarningExpression();
        }

        return propertySyntax;
    }

    private MethodDeclarationSyntax CreateLifecycleMethodMethod(
        string methodName,
        IReadOnlyList<PropertyDescriptor> arguments)
    {
        var argumentList = new List<ArgumentSyntax>();

        foreach (var argument in arguments)
        {
            argumentList.Add(Argument(IdentifierName(GetPropertyName(argument.Name))));
        }

        argumentList.Add(
            Argument(IdentifierName("Strategy"))
                .WithNameColon(NameColon(IdentifierName("strategy"))));

        var bodyStatements =
            SingletonList<StatementSyntax>(
                ExpressionStatement(
                    InvocationExpression(
                            IdentifierName("Subscribe"))
                        .WithArgumentList(
                            ArgumentList(
                                SingletonSeparatedList(
                                    Argument(
                                        InvocationExpression(
                                                MemberAccessExpression(
                                                    SyntaxKind.SimpleMemberAccessExpression,
                                                    IdentifierName("Operation"),
                                                    IdentifierName("Watch")))
                                            .WithArgumentList(
                                                ArgumentList(
                                                    SeparatedList(argumentList)))))))));

#if NET8_0
        return MethodDeclaration(
                PredefinedType(Token(SyntaxKind.VoidKeyword)),
                Identifier(methodName))
            .WithModifiers(
                TokenList(
                    Token(SyntaxKind.ProtectedKeyword),
                    Token(SyntaxKind.OverrideKeyword)))
            .WithBody(Block(bodyStatements));
#else
        var ifStatement =
            IfStatement(
                MemberAccessExpression(
                    SyntaxKind.SimpleMemberAccessExpression,
                    IdentifierName("RendererInfo"),
                    IdentifierName("IsInteractive")),
                Block(bodyStatements));

        return MethodDeclaration(
                PredefinedType(Token(SyntaxKind.VoidKeyword)),
                Identifier(methodName))
            .WithModifiers(
                TokenList(
                    Token(SyntaxKind.ProtectedKeyword),
                    Token(SyntaxKind.OverrideKeyword)))
            .WithBody(Block(ifStatement));
#endif
    }

    private MethodDeclarationSyntax CreateOnInitializedAsyncMethod(
        OperationDescriptor descriptor)
    {
        var argumentList = new List<ArgumentSyntax>();
 
        foreach (var argument in descriptor.Arguments)
        {
            argumentList.Add(Argument(IdentifierName(GetPropertyName(argument.Name))));
        }
 
        var watchArgumentList = argumentList.Append(
            Argument(IdentifierName("Strategy"))
                .WithNameColon(NameColon(IdentifierName("strategy"))));
 
        var subscribe = ExpressionStatement(
            InvocationExpression(
                IdentifierName("Subscribe"))
                .WithArgumentList(
                    ArgumentList(
                        SingletonSeparatedList(
                            Argument(
                                InvocationExpression(
                                    MemberAccessExpression(
                                        SyntaxKind.SimpleMemberAccessExpression,
                                        IdentifierName("Operation"),
                                        IdentifierName("Watch")))
                                .WithArgumentList(
                                    ArgumentList(
                                        SeparatedList(watchArgumentList))))))));


        var executeStatement =
            LocalDeclarationStatement(
                VariableDeclaration(IdentifierName(Identifier(TriviaList(), SyntaxKind.VarKeyword, "var", "var", TriviaList())))
                .WithVariables(
                    SingletonSeparatedList(
                        VariableDeclarator(Identifier("result"))
                        .WithInitializer(
                            EqualsValueClause(
                                AwaitExpression(
                                    InvocationExpression(
                                        MemberAccessExpression(
                                            SyntaxKind.SimpleMemberAccessExpression,
                                            IdentifierName("Operation"),
                                            IdentifierName("ExecuteAsync")))
                                        .WithArgumentList(
                                            ArgumentList(
                                                SeparatedList(argumentList
                                                    .Append(
                                                        Argument(IdentifierName("CancellationToken"))
                                                            .WithNameColon(NameColon(IdentifierName("cancellationToken")))))))))))));

        var subscribeReturn =
            ExpressionStatement(
                InvocationExpression(
                    IdentifierName("Subscribe"))
                    .WithArgumentList(
                        ArgumentList(
                            SingletonSeparatedList(
                                Argument(
                                    InvocationExpression(
                                        MemberAccessExpression(
                                            SyntaxKind.SimpleMemberAccessExpression,
                                            IdentifierName("global::System.Reactive.Linq.Observable"),
                                            IdentifierName("Return")))
                                    .WithArgumentList(
                                        ArgumentList(
                                            SingletonSeparatedList(
                                                Argument(IdentifierName("result"))))))))));
 
        var ifHydrateThenRestore =
            IfStatement(
                IdentifierName("Hydrate"),
                Block(
                    SingletonList<StatementSyntax>(
                        ExpressionStatement(
                            InvocationExpression(
                                MemberAccessExpression(
                                    SyntaxKind.SimpleMemberAccessExpression,
                                    IdentifierName("HydrateService"),
                                    IdentifierName("Restore")))
                            .WithArgumentList(
                                ArgumentList(
                                    SeparatedList([
                                        Argument(
                                            LiteralExpression(
                                                SyntaxKind.StringLiteralExpression,
                                                Literal(descriptor.Name))),
                                        Argument(
                                            MemberAccessExpression(
                                                SyntaxKind.SimpleMemberAccessExpression,
                                                IdentifierName("ResultBuilder"),
                                                IdentifierName("BuildData")))])))))));
        var ifHydrateThenPersist
            = IfStatement(
                IdentifierName("Hydrate"),
                Block(
                    LocalDeclarationStatement(
                        VariableDeclaration(
                            IdentifierName(
                                Identifier(
                                    TriviaList(),
                                    SyntaxKind.VarKeyword,
                                    "var",
                                    "var",
                                    TriviaList())))
                        .WithVariables(
                            SingletonSeparatedList(
                                VariableDeclarator(
                                    Identifier("request"))
                                .WithInitializer(
                                    EqualsValueClause(
                                        InvocationExpression(
                                            MemberAccessExpression(
                                                SyntaxKind.SimpleMemberAccessExpression,
                                                IdentifierName("Operation"),
                                                IdentifierName("CreateRequest")))
                                        .WithArgumentList(ArgumentList(SeparatedList(argumentList)))))))),
                    ExpressionStatement(
                        InvocationExpression(
                            MemberAccessExpression(
                                SyntaxKind.SimpleMemberAccessExpression,
                                IdentifierName("HydrateService"),
                                IdentifierName("Persist")))
                        .WithArgumentList(
                            ArgumentList(
                                SeparatedList([
                                    Argument(
                                        IdentifierName("request")),
                                    Argument(
                                        IdentifierName("result"))]))))));

        var ifInteractive =
            IfStatement(
                MemberAccessExpression(
                    SyntaxKind.SimpleMemberAccessExpression,
                    IdentifierName("RendererInfo"),
                    IdentifierName("IsInteractive")),
                Block(
                    ifHydrateThenRestore,
                    subscribe),
                ElseClause(
                    Block(
                        executeStatement,
                        ifHydrateThenPersist,
                        subscribeReturn)));
 
        return MethodDeclaration(
            ParseTypeName(TypeNames.Task),
            Identifier("OnInitializedAsync"))
            .WithModifiers(
                TokenList(
                    Token(SyntaxKind.ProtectedKeyword),
                    Token(SyntaxKind.OverrideKeyword),
                    Token(SyntaxKind.AsyncKeyword)))
            .WithBody(Block(ifInteractive));
    }
}
