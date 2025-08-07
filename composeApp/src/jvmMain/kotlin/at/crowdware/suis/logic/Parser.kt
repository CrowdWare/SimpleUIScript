package at.crowdware.suis.logic

import com.github.h0tk3y.betterParse.combinators.*
import com.github.h0tk3y.betterParse.grammar.Grammar
import com.github.h0tk3y.betterParse.grammar.parseToEnd
import com.github.h0tk3y.betterParse.grammar.parser
import com.github.h0tk3y.betterParse.lexer.literalToken
import com.github.h0tk3y.betterParse.lexer.regexToken
import com.github.h0tk3y.betterParse.parser.Parser

// AST-Knoten
sealed class ASTNode

sealed class Statement : ASTNode()

// Statements
data class IfStatement(
    val condition: Expression,
    val thenBranch: List<Statement>,
    val elseBranch: List<Statement>?
) : Statement()

data class VarDeclaration(val name: String, val value: Expression) : Statement()
data class Assignment(val name: String, val value: Expression) : Statement()
data class ExpressionStatement(val expression: Expression) : Statement()

data class WhileStatement(
    val condition: Expression,
    val body: List<Statement>
) : Statement()

// Neue for-Schleife
data class ForStatement(
    val init: Statement?,
    val condition: Expression?,
    val update: Statement?,
    val body: List<Statement>
) : Statement()

// Expressions
sealed class Expression : ASTNode()
data class UnaryExpression(val operator: String, val expr: Expression) : Expression()
data class BinaryExpression(val left: Expression, val operator: String, val right: Expression) : Expression()
data class FunctionCall(val name: String, val arguments: List<Expression>) : Expression()
data class StringLiteral(val value: String) : Expression()
data class NumberLiteral(val value: Int) : Expression()
data class BooleanLiteral(val value: Boolean) : Expression()

data class Identifier(val name: String) : Expression()

// Postfix-Inkrement
data class PostfixExpression(
    val expr: Expression,
    val operator: String
) : Expression()

// Grammar Definition
object MiniLanguageGrammar : Grammar<List<Statement>>() {
    // WICHTIG: Whitespace und Kommentare zuerst definieren
    val WS by regexToken("\\s+", ignore = true)
    val LINE_COMMENT by regexToken("//[^\\r\\n]*", ignore = true)

    // Multi-character operators first (längere Tokens haben Vorrang)
    val EQUALS by literalToken("==")
    val NOT_EQUALS by literalToken("!=")
    val LE by literalToken("<=")
    val GE by literalToken(">=")
    val PP by literalToken("++")
    val MM by literalToken("--")  // Postfix-Dekrement hinzugefügt
    val AND by literalToken("&&")
    val OR by literalToken("||")

    // Keywords
    val VAR by literalToken("var")
    val IF by literalToken("if")
    val ELSE by literalToken("else")
    val WHILE by literalToken("while")
    val FOR by literalToken("for")
    val TRUE by literalToken("true")
    val FALSE by literalToken("false")

    // Single character tokens
    val LT by literalToken("<")
    val GT by literalToken(">")
    val ASSIGN by literalToken("=")
    val PLUS by literalToken("+")
    val MINUS by literalToken("-")
    val MULT by literalToken("*")
    val DIV by literalToken("/")
    val NOT by literalToken("!")
    val LBRACE by literalToken("{")
    val RBRACE by literalToken("}")
    val LPAREN by literalToken("(")
    val RPAREN by literalToken(")")
    val COMMA by literalToken(",")
    val SEMICOLON by literalToken(";")

    // Regex tokens
    val STRING by regexToken("\"[^\"]*\"")
    val NUMBER by regexToken("\\d+")
    val IDENTIFIER by regexToken("[a-zA-Z_][a-zA-Z0-9_]*")

    // Parser Declarations
    val expression: Parser<Expression> by parser { logicalExpression }

    // Basic Parsers
    val identifier by IDENTIFIER use { Identifier(text) }
    val stringLiteral by STRING use { StringLiteral(text.substring(1, text.length - 1)) }
    val numberLiteral by NUMBER use { NumberLiteral(text.toInt()) }
    val booleanLiteral by (TRUE or FALSE) use { BooleanLiteral(text.toBoolean()) }
    val parenthesizedExpression by (-LPAREN and expression and -RPAREN)
    val argumentList by separatedTerms(expression, COMMA, acceptZero = true)
    val functionCall by (IDENTIFIER and -LPAREN and argumentList and -RPAREN) use { FunctionCall(t1.text, t2) }
    val primaryExpression by (
            functionCall or stringLiteral or numberLiteral or booleanLiteral or identifier or parenthesizedExpression
            )

    // Postfix-Inkrement/Dekrement-Parser
    val postfixExpression: Parser<Expression> by parser {
        primaryExpression and optional(PP or MM)
    }.map { (expr, op) ->
        if (op != null) PostfixExpression(expr, op.text) else expr
    }

    // Operator Precedence
    // Unäre Operatoren mit map
    val unaryExpression: Parser<Expression> by parser {
        ((NOT or PLUS or MINUS) and unaryExpression).map { (token, expr) ->
            UnaryExpression(token.text, expr)
        } or postfixExpression
    }
    val multiplicativeExpression by leftAssociative(unaryExpression, MULT or DIV) { l, op, r ->
        BinaryExpression(l, op.text, r)
    }
    val additiveExpression by leftAssociative(multiplicativeExpression, PLUS or MINUS) { l, op, r ->
        BinaryExpression(l, op.text, r)
    }
    val relationalExpression by leftAssociative(additiveExpression, LE or GE or LT or GT) { l, op, r ->
        BinaryExpression(l, op.text, r)
    }
    val equalityExpression by leftAssociative(relationalExpression, EQUALS or NOT_EQUALS) { l, op, r ->
        BinaryExpression(l, op.text, r)
    }
    val andExpression by leftAssociative(equalityExpression, AND) { l, _, r ->
        BinaryExpression(l, "&&", r)
    }
    val logicalExpression by leftAssociative(andExpression, OR) { l, _, r ->
        BinaryExpression(l, "||", r)
    }

    // Statements
    val varDeclaration by (-VAR and IDENTIFIER and -ASSIGN and expression) use { VarDeclaration(t1.text, t2) }
    val assignment by (IDENTIFIER and -ASSIGN and expression) use { Assignment(t1.text, t2) }
    val expressionStatement by expression use { ExpressionStatement(this) }
    val blockStatements: Parser<List<Statement>> by parser {
        separatedTerms(statement, WS, acceptZero = true)
    }
    val elseClause by (-ELSE and -LBRACE and blockStatements and -RBRACE)
    val ifWithParens by (
            -IF and -LPAREN and expression and -RPAREN and
                    -LBRACE and blockStatements and -RBRACE and optional(elseClause)
            ) use {
        val condition = t1
        val thenBranch = t2
        val elseBranch = t3
        IfStatement(condition, thenBranch, elseBranch)
    }
    val ifWithoutParens by (
            -IF and expression and -LBRACE and blockStatements and -RBRACE and
                    optional(elseClause)
            ) use {
        val condition = t1
        val thenBranch = t2
        val elseBranch = t3
        IfStatement(condition, thenBranch, elseBranch)
    }
    val ifStatement: Parser<Statement> by parser {
        ifWithParens or ifWithoutParens
    }
    val whileStatement: Parser<Statement> by parser {
        -WHILE and -LPAREN and expression and -RPAREN and
                -LBRACE and blockStatements and -RBRACE
    }.map { (cond, body) ->
        WhileStatement(cond, body)
    }

    // For-Schleife Parser
    val forStatement: Parser<Statement> by parser {
        -FOR and -LPAREN and
                optional(varDeclaration or assignment) and -SEMICOLON and
                optional(expression) and -SEMICOLON and
                optional(assignment or expressionStatement) and -RPAREN and
                -LBRACE and blockStatements and -RBRACE
    }.map { (init, condition, update, body) ->
        ForStatement(init, condition, update, body)
    }

    val statement: Parser<Statement> by parser {
        varDeclaration or assignment or ifStatement or whileStatement or forStatement or expressionStatement
    }
    override val rootParser by separatedTerms(statement, WS, acceptZero = true)
}

// Interpreter
class Interpreter {
    private val variables = mutableMapOf<String, Any>()

    init {
        variables["name"] = "Art"
    }

    fun interpret(statements: List<Statement>) {
        statements.forEach { executeStatement(it) }
    }

    private fun executeStatement(statement: Statement) {
        when (statement) {
            is VarDeclaration -> {
                val value = evaluateExpression(statement.value)
                variables[statement.name] = value ?: Unit
                println("Variable '${statement.name}' = $value")
            }
            is Assignment -> {
                val value = evaluateExpression(statement.value)
                variables[statement.name] = value ?: Unit
                println("Variable '${statement.name}' zugewiesen = $value")
            }
            is IfStatement -> {
                val conditionResult = evaluateExpression(statement.condition)
                if (conditionResult == true) {
                    statement.thenBranch.forEach { executeStatement(it) }
                } else {
                    statement.elseBranch?.forEach { executeStatement(it) }
                }
            }
            is WhileStatement -> {
                while ((evaluateExpression(statement.condition) as Boolean)) {
                    statement.body.forEach { executeStatement(it) }
                }
            }
            // Neue for-Schleife Ausführung
            is ForStatement -> {
                // Initialisierung
                statement.init?.let { executeStatement(it) }

                // Schleife
                while (true) {
                    // Bedingung prüfen (wenn keine Bedingung vorhanden, endlos laufen)
                    if (statement.condition != null) {
                        val conditionResult = evaluateExpression(statement.condition)
                        if (conditionResult != true) break
                    }

                    // Schleifenkörper ausführen
                    statement.body.forEach { executeStatement(it) }

                    // Update-Statement ausführen
                    statement.update?.let { executeStatement(it) }
                }
            }
            is ExpressionStatement -> {
                evaluateExpression(statement.expression)
            }
        }
    }

    private fun evaluateExpression(expression: Expression): Any? {
        return when (expression) {
            // Handle postfix increment/decrement
            is PostfixExpression -> {
                val id = expression.expr as? Identifier
                    ?: error("'++/--' only on identifiers")
                val old = (variables[id.name] as? Int)
                    ?: error("Cannot ++/-- non-int variable")
                when (expression.operator) {
                    "++" -> {
                        variables[id.name] = old + 1
                        old
                    }
                    "--" -> {
                        variables[id.name] = old - 1
                        old
                    }
                    else -> error("Unknown postfix operator: ${expression.operator}")
                }
            }
            is UnaryExpression -> {
                val v = evaluateExpression(expression.expr)
                when (expression.operator) {
                    "!" -> (v as? Boolean)?.not() ?: error("'!' only for booleans")
                    "+" -> (v as? Int) ?: error("'+' only for numbers")
                    "-" -> (v as? Int)?.let { -it } ?: error("'-' only for numbers")
                    else -> error("Unknown unary operator: ${expression.operator}")
                }
            }
            is BinaryExpression -> {
                val left = evaluateExpression(expression.left)
                val right = evaluateExpression(expression.right)
                when (expression.operator) {
                    "==" -> left == right
                    "!=" -> left != right
                    "<"  -> (left as? Int ?: error("'<' only for numbers")) < (right as Int)
                    ">"  -> (left as? Int ?: error("'>' only for numbers")) > (right as Int)
                    "<=" -> (left as? Int ?: error("'<=' only for numbers")) <= (right as Int)
                    ">=" -> (left as? Int ?: error("'>=' only for numbers")) >= (right as Int)
                    "*"  -> (left as? Int)?.let { l -> (right as? Int)?.let { r -> l * r } } ?: error("'*' only for numbers")
                    "/"  -> (left as? Int)?.let { l -> (right as? Int)?.let { r -> l / r } } ?: error("'/' only for numbers")
                    "+"  -> when {
                        left is String || right is String -> "$left$right"
                        left is Int && right is Int -> left + right
                        else -> error("'+' only for numbers or strings")
                    }
                    "-"  -> (left as? Int)?.let { l -> (right as? Int)?.let { r -> l - r } } ?: error("'-' only for numbers")
                    "&&" -> (left == true) && (right == true)
                    "||" -> (left == true) || (right == true)
                    else  -> error("Unknown operator: ${expression.operator}")
                }
            }
            is FunctionCall   -> {
                val args = expression.arguments.map { evaluateExpression(it) }
                executeFunction(expression.name, args)
            }
            is StringLiteral  -> expression.value
            is NumberLiteral  -> expression.value
            is BooleanLiteral -> expression.value
            is Identifier     -> variables[expression.name]
        }
    }

    private fun executeFunction(name: String, args: List<Any?>): Any? {
        return when (name) {
            "submit"    -> { println("submit() wurde aufgerufen"); Unit }
            "showAlert" -> { println("Alert: ${args.firstOrNull() ?: "No message"}"); Unit }
            else         -> error("Unknown function: $name")
        }
    }

    fun setVariable(name: String, value: Any) {
        variables[name] = value
    }

    fun getVariable(name: String): Any? = variables[name]
}

fun test() {
    val code = """
        var a = 4
        var b = 2
        var c = a * b * 4 / 2
        var d = a / b
        var flag = false
        var neg = -a
        var pos = +a
        var inv = !flag
        
        showAlert("c = " + c + ", d = " + d)
        showAlert("neg = " + neg + ", pos = " + pos + ", inv = " + inv)

        if (c == 8 && d == 2) {
            showAlert("Multiplikation und Division funktionieren!")
        }

        if (!flag && neg < 0) {
            showAlert("Unary operations work!")
        }
     
        var count = 0
        while (count < 3) {
            showAlert("While Count = " + count)
            count++
        }
        
        // Neue for-Schleife Tests
        showAlert("Testing for-loop:")
        for (var i = 0; i < 5; i++) {
            showAlert("For loop i = " + i)
        }
        
        // for-Schleife mit externen Variablen
        showAlert("For loop with external variables:")
        var j = 10
        for (; j > 5; j = j - 1) {
            showAlert("For loop j = " + j)
        }
        
        // for-Schleife ohne Initialisierung
        showAlert("For loop starting from existing variable:")
        var k = 0
        for (; k <= 2; k++) {
            showAlert("For loop k = " + k)
        }
        
        // for-Schleife mit Dekrement
        showAlert("For loop with decrement:")
        for (var m = 5; m > 0; m--) {
            showAlert("For loop m = " + m)
        }
    """.trimIndent()

    try {
        val ast = MiniLanguageGrammar.parseToEnd(code)
        val interpreter = Interpreter()

        println("Parsing erfolgreich. AST:")
        ast.forEach { println("  $it") }
        println("\nAusführung:")

        interpreter.interpret(ast)

        println("\nVariablen nach Ausführung:")
        println("a = ${interpreter.getVariable("a")}")
        println("b = ${interpreter.getVariable("b")}")
        println("c = ${interpreter.getVariable("c")}")
        println("d = ${interpreter.getVariable("d")}")
        println("neg = ${interpreter.getVariable("neg")}")
        println("pos = ${interpreter.getVariable("pos")}")
        println("inv = ${interpreter.getVariable("inv")}")
        println("count = ${interpreter.getVariable("count")}")
        println("i = ${interpreter.getVariable("i")}")
        println("j = ${interpreter.getVariable("j")}")
        println("k = ${interpreter.getVariable("k")}")
        println("name = ${interpreter.getVariable("name")}")

    } catch (e: Exception) {
        println("Fehler beim Parsen oder Ausführen: ${e.message}")
        e.printStackTrace()
    }
}

// TODO:
// - Blöcke mit deren Scopes {}
// - Benutzerdefinierte Funktionen
// - Kommentare
// - Fehlerbehandlung mit Zeilennummer, Spalte
// - break/continue für Schleifen