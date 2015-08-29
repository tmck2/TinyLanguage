module IlGenerator

open Il
open Parser
open Railway

let private codegenOperator = function
| Plus  -> Instruction.Add
| Minus -> Instruction.Sub
| Times -> Instruction.Mul

let rec private codegenExpression (expression : Expression ) =
    match expression with
    | ConstantInt n -> [ Ldc_I4 n ]
    | Invoke (Builtin operation, expressions) ->
        let argumentCount = List.length expressions
        let arguments = expressions |> List.collect codegenExpression
        let invokes = List.replicate (argumentCount - 1) (codegenOperator operation)
        arguments @ invokes
    | wrong -> failwithf "Sorry, you can't pass %A here." wrong

let private findErrors(expressions : Expression list): Result<Expression list, string> =
    match expressions |> List.collect findAllErrors with
    | [] -> succeed expressions
    | errors -> fail (String.concat System.Environment.NewLine errors)

let private codegenDefun (expression : Expression): (string * Instruction list) =
    match expression with
    | Defun (name, bodyExpressions) -> name, bodyExpressions |> List.collect codegenExpression
    | wrong -> failwithf "Expected Defun, found %A." wrong

let codegenDefuns (expressions : Expression list): Result<Map<string, Instruction list>, string> = 
    expressions
        |> List.map codegenDefun
        |> Map.ofList
        |> succeed

let codegen (expressions : Expression list): Result<Map<string, Instruction list>, string> = 
    expressions
        |> findErrors
        >>= codegenDefuns