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
// CHG: Assignment to a member (e.g., obj.field = expr)
data class MemberAssignment(val target: MemberAccess, val value: Expression) : Statement()
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

// Break und Continue Statements
data class BreakStatement(val dummy: Unit = Unit) : Statement()
data class ContinueStatement(val dummy: Unit = Unit) : Statement()

// Return Statement
data class ReturnStatement(val value: Expression?) : Statement()
// CHG: Data class declaration support
data class DataClassDeclaration(
    val name: String,
    val fields: List<String>
) : Statement()

// Function Definition
data class FunctionDeclaration(
    val name: String,
    val parameters: List<String>,
    val body: List<Statement>
) : Statement()

// Exception-Klassen für Kontrollfluss
class BreakException : Exception()
class ContinueException : Exception()
class ReturnException(val value: Any?) : Exception()

// Expressions
sealed class Expression : ASTNode()
data class UnaryExpression(val operator: String, val expr: Expression) : Expression()
data class BinaryExpression(val left: Expression, val operator: String, val right: Expression) : Expression()
data class FunctionCall(val name: String, val arguments: List<Expression>) : Expression()
data class StringLiteral(val value: String) : Expression()
data class NumberLiteral(val value: Int) : Expression()
data class BooleanLiteral(val value: Boolean) : Expression()

data class Identifier(val name: String) : Expression()
// CHG: Member access expression (e.g., obj.field)
data class MemberAccess(val receiver: Expression, val member: String) : Expression()

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
    // Neu: Blockkommentare /* */ - unterstützt auch mehrzeilige Kommentare
    val BLOCK_COMMENT by regexToken("/\\*[\\s\\S]*?\\*/", ignore = true)

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
    val BREAK by literalToken("break")
    val CONTINUE by literalToken("continue")
    val FUN by literalToken("fun")
    val RETURN by literalToken("return")
    val TRUE by literalToken("true")
    val FALSE by literalToken("false")
    // CHG: data classes keyword
    val DATA by literalToken("data")
    // CHG: Kotlin-style 'class' keyword for 'data class'
    val CLASS by literalToken("class")

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
    // CHG: dot operator for member access
    val DOT by literalToken(".")
    val SEMICOLON by literalToken(";")

    // Regex tokens
    val STRING by regexToken("\"[^\"]*\"")
    val NUMBER by regexToken("\\d+")
    val IDENTIFIER by regexToken("[a-zA-Z_][a-zA-Z0-9_]*")

    // Parser Declarations
    // CHG: top-level expression now goes through logicalExpression (unchanged)
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

    // CHG: (-DOT and IDENTIFIER) yields IDENTIFIER TokenMatch in this BetterParse version
    val memberTail: Parser<String> = (-DOT and IDENTIFIER).map { id ->
        (id as com.github.h0tk3y.betterParse.lexer.TokenMatch).text
    }
    val memberExpression: Parser<Expression> =
        (postfixExpression and optional<String>(memberTail))
            .map { pair ->
                val expr: Expression = pair.t1
                val name: String? = pair.t2
                if (name != null) MemberAccess(expr, name) else expr
            }

    // Operator Precedence
    // CHG: unary now sits above memberExpression
    val unaryExpression: Parser<Expression> by parser {
        ((NOT or PLUS or MINUS) and unaryExpression).map { (token, expr) ->
            UnaryExpression(token.text, expr)
        } or memberExpression
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
    // CHG: assignment supports identifiers and member targets
    val assignment: Parser<Statement> by parser {
        (memberExpression and -ASSIGN and expression).map { (lhs, rhs) ->
            when (lhs) {
                is Identifier -> Assignment(lhs.name, rhs)
                is MemberAccess -> MemberAssignment(lhs, rhs)
                else -> error("Invalid assignment target")
            }
        }
    }
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

    // Break und Continue Statements
    val breakStatement by BREAK use { BreakStatement() }
    val continueStatement by CONTINUE use { ContinueStatement() }

    // Return Statement
    val returnStatement by (RETURN and optional(expression)) use { ReturnStatement(t2) }

    // Function Declaration
    val parameterList by separatedTerms(IDENTIFIER, COMMA, acceptZero = true)
    val functionDeclaration: Parser<Statement> by (
        -FUN and IDENTIFIER and -LPAREN and parameterList and -RPAREN and -LBRACE and blockStatements and -RBRACE
    ) use {
        // CHG: simplified tuple after skipping punctuation and 'fun'
        FunctionDeclaration(t1.text, t2.map { it.text }, t3)
    }

    // CHG: Kotlin-style syntax: 'data class Name(field1, field2)'
    val dataClassDeclaration: Parser<Statement> by (
        -DATA and -CLASS and IDENTIFIER and -LPAREN and parameterList and -RPAREN
    ) use {
        DataClassDeclaration(t1.text, t2.map { it.text })
    }

    val statement: Parser<Statement> by parser {
        functionDeclaration or dataClassDeclaration or varDeclaration or assignment or ifStatement or whileStatement or forStatement or
                breakStatement or continueStatement or returnStatement or expressionStatement
    }
    override val rootParser by separatedTerms(statement, WS, acceptZero = true)
}

// Interpreter
class Interpreter {
    private val variables = mutableMapOf<String, Any>()
    private val functions = mutableMapOf<String, FunctionDeclaration>()
    // CHG: registry for data classes
    private val dataClasses = mutableMapOf<String, DataClassDeclaration>()

    // CHG: runtime instance for data classes
    data class DataInstance(val className: String, val fields: MutableMap<String, Any?>) {
        override fun toString(): String = "${'$'}className${'$'}fields"
    }

    init {
        variables["name"] = "Art"
    }

    fun interpret(statements: List<Statement>) {
        // Erste Phase: Funktionsdeklarationen sammeln
        statements.forEach { statement ->
            when (statement) {
                is FunctionDeclaration -> functions[statement.name] = statement
                is DataClassDeclaration -> dataClasses[statement.name] = statement
                else -> {}
            }
        }

        // Zweite Phase: Code ausführen
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
            // CHG: member assignment
            is MemberAssignment -> {
                val target = evaluateExpression(statement.target.receiver)
                val value = evaluateExpression(statement.value)
                val inst = target as? DataInstance ?: error("Member assignment on non-object")
                inst.fields[statement.target.member] = value
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
                try {
                    while ((evaluateExpression(statement.condition) as Boolean)) {
                        try {
                            statement.body.forEach { executeStatement(it) }
                        } catch (e: ContinueException) {
                            // Continue zur nächsten Iteration
                            continue
                        }
                    }
                } catch (e: BreakException) {
                    // Break aus der Schleife
                }
            }
            // Neue for-Schleife Ausführung
            is ForStatement -> {
                try {
                    // Initialisierung
                    statement.init?.let { executeStatement(it) }

                    // Schleife
                    while (true) {
                        // Bedingung prüfen (wenn keine Bedingung vorhanden, endlos laufen)
                        if (statement.condition != null) {
                            val conditionResult = evaluateExpression(statement.condition)
                            if (conditionResult != true) break
                        }

                        try {
                            // Schleifenkörper ausführen
                            statement.body.forEach { executeStatement(it) }
                        } catch (e: ContinueException) {
                            // Continue - springe zum Update und zur nächsten Iteration
                        }

                        // Update-Statement ausführen
                        statement.update?.let { executeStatement(it) }
                    }
                } catch (e: BreakException) {
                    // Break aus der Schleife
                }
            }
            is BreakStatement -> {
                throw BreakException()
            }
            is ContinueStatement -> {
                throw ContinueException()
            }
            is ReturnStatement -> {
                val value = statement.value?.let { evaluateExpression(it) }
                throw ReturnException(value)
            }
            is FunctionDeclaration -> {
                // Funktionsdeklarationen werden bereits in interpret() verarbeitet
                // Hier nichts tun
            }
            is DataClassDeclaration -> {
                // Already handled in interpret()
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
            // CHG: member access
            is MemberAccess -> {
                val recv = evaluateExpression(expression.receiver)
                val inst = recv as? DataInstance ?: error("Member access on non-object")
                inst.fields[expression.member]
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
            is StringLiteral  -> {
                // CHG: Kotlin-style string interpolation: "...${expr}..."
                val s = expression.value
                if (!s.contains("\${")) return s
                val regex = Regex("\\$\\{([^}]*)}" )
                var out = StringBuilder()
                var lastIndex = 0
                for (m in regex.findAll(s)) {
                    out.append(s.substring(lastIndex, m.range.first))
                    val inside = m.groupValues[1].trim()
                    val value = evaluateTemplateExpr(inside) // CHG: removed selection artifacts
                    out.append(value?.toString() ?: "null")
                    lastIndex = m.range.last + 1
                }
                out.append(s.substring(lastIndex))
                out.toString()
            }
            is NumberLiteral  -> expression.value
            is BooleanLiteral -> expression.value
            is Identifier     -> variables[expression.name]
        }
    }

    // CHG: expression evaluator for ${ ... } supporting +, -, *, /, parentheses, identifiers and member access
    private fun evaluateTemplateExpr(text: String): Any? {
        // --- Lexer ---
        data class Tok(val type: String, val s: String)
        val src = text.trim()
        val toks = mutableListOf<Tok>()
        var i = 0
        fun isIdStart(c: Char) = c == '_' || c.isLetter()
        fun isIdPart(c: Char) = c == '_' || c.isLetterOrDigit()
        while (i < src.length) {
            val c = src[i]
            when {
                c.isWhitespace() -> { i++ }
                c.isDigit() -> {
                    val start = i
                    while (i < src.length && src[i].isDigit()) i++
                    toks += Tok("NUM", src.substring(start, i))
                }
                isIdStart(c) -> {
                    val start = i
                    i++
                    while (i < src.length && isIdPart(src[i])) i++
                    toks += Tok("ID", src.substring(start, i))
                }
                c == '"' -> {
                    // simple string literal support inside template
                    val start = ++i
                    while (i < src.length && src[i] != '"') i++
                    val lit = src.substring(start, i)
                    if (i < src.length && src[i] == '"') i++
                    toks += Tok("STR", lit)
                }
                c == '.' -> { toks += Tok("DOT", "."); i++ }
                c == '+' -> { toks += Tok("PLUS", "+"); i++ }
                c == '-' -> { toks += Tok("MINUS", "-"); i++ }
                c == '*' -> { toks += Tok("MUL", "*"); i++ }
                c == '/' -> { toks += Tok("DIV", "/"); i++ }
                c == '(' -> { toks += Tok("LP", "("); i++ }
                c == ')' -> { toks += Tok("RP", ")"); i++ }
                else -> {
                    // unsupported char -> treat as text
                    return src
                }
            }
        }
        var p = 0
        fun peek(t: String) = p < toks.size && toks[p].type == t
        fun eat(t: String): Tok { val tok = toks.getOrNull(p); require(tok != null && tok.type == t) {"Expected $t"}; p++; return tok }

        // --- Parser (recursive descent) ---
        fun parseExpr(): Any? {
            fun parseTerm(): Any? {
                fun parseFactor(): Any? {
                    when {
                        peek("NUM") -> return eat("NUM").s.toInt()
                        peek("STR") -> return eat("STR").s
                        peek("LP") -> { eat("LP"); val v = parseExpr(); eat("RP"); return v }
                        peek("ID") -> {
                            var cur: Any? = variables[eat("ID").s]
                            while (peek("DOT")) {
                                eat("DOT")
                                val name = eat("ID").s
                                cur = when (cur) {
                                    is DataInstance -> cur.fields[name]
                                    else -> return null
                                }
                            }
                            return cur
                        }
                        peek("MINUS") -> { eat("MINUS"); val v = parseFactor(); return when (v) { is Int -> -v; is String -> v; else -> null } }
                        peek("PLUS") -> { eat("PLUS"); return parseFactor() }
                        else -> return null
                    }
                }
                var left = parseFactor()
                while (peek("MUL") || peek("DIV")) {
                    val op = toks[p++].type
                    val right = parseFactor()
                    left = when (op) {
                        "MUL" -> (left as? Int)?.let { l -> (right as? Int)?.let { r -> l * r } } ?: return null
                        "DIV" -> (left as? Int)?.let { l -> (right as? Int)?.let { r -> if (r == 0) return null else l / r } } ?: return null
                        else -> left
                    }
                }
                return left
            }
            var left = parseTerm()
            while (peek("PLUS") || peek("MINUS")) {
                val op = toks[p++].type
                val right = parseTerm()
                left = when (op) {
                    "PLUS" -> when {
                        left is String || right is String -> "$left$right"
                        left is Int && right is Int -> left + right
                        else -> return null
                    }
                    "MINUS" -> (left as? Int)?.let { l -> (right as? Int)?.let { r -> l - r } } ?: return null
                    else -> left
                }
            }
            return left
        }

        return parseExpr()
    }

    private fun executeFunction(name: String, args: List<Any?>): Any? {
        return when (name) {
            "submit"    -> { println("submit() wurde aufgerufen"); Unit }
            "showAlert" -> { println("Alert: ${args.firstOrNull() ?: "No message"}"); Unit }
            else -> {
                // CHG: data class constructor call
                dataClasses[name]?.let { dc ->
                    if (args.size != dc.fields.size) error("Data class '${'$'}name' expects ${'$'}{dc.fields.size} args, got ${'$'}{args.size}")
                    val fieldMap = mutableMapOf<String, Any?>()
                    dc.fields.forEachIndexed { idx, f -> fieldMap[f] = args[idx] }
                    return DataInstance(dc.name, fieldMap)
                }
                // Benutzerdefinierte Funktion
                val function = functions[name] ?: error("Unknown function: ${'$'}name")

                // Parameter-Anzahl prüfen
                if (args.size != function.parameters.size) {
                    error("Function '${'$'}name' expects ${'$'}{function.parameters.size} arguments, got ${'$'}{args.size}")
                }

                // Neuen Scope für Funktionsausführung erstellen
                val savedVariables = variables.toMap()

                try {
                    // Parameter als lokale Variablen setzen
                    function.parameters.forEachIndexed { index, param ->
                        variables[param] = args[index] ?: Unit
                    }

                    // Funktionskörper ausführen
                    function.body.forEach { executeStatement(it) }

                    // Standardrückgabe wenn kein explizites return
                    null

                } catch (e: ReturnException) {
                    // Return-Statement gefangen
                    e.value
                } finally {
                    // Originale Variablen wiederherstellen (außer globale)
                    variables.clear()
                    variables.putAll(savedVariables)
                }
            }
        }
    }

    fun setVariable(name: String, value: Any) {
        variables[name] = value
    }

    fun getVariable(name: String): Any? = variables[name]
}

fun test() {
    val code = """
        /* Dies ist ein mehrzeiliger 
           Blockkommentar der verschiedene 
           Zeilen umfasst */
           
        /* Benutzerdefinierte Funktionen */
        fun add(a, b) {
            return a + b
        }
        
        fun greet(name) {
            showAlert("Hello, " + name + "!")
            return "Greeting sent to " + name
        }
        
        fun factorial(n) {
            if (n <= 1) {
                return 1
            }
            return n * factorial(n - 1)
        }
        
        fun fibonacci(n) {
            if (n <= 1) {
                return n
            }
            return fibonacci(n - 1) + fibonacci(n - 2)
        }
        
        fun countDown(n) {
            while (n > 0) {
                showAlert("Countdown: " + n)
                n = n - 1
                if (n == 3) {
                    return "Early exit at " + n
                }
            }
            return "Countdown finished"
        }
        
        /* Teste benutzerdefinierte Funktionen */
        var result1 = add(5, 3)
        showAlert("add(5, 3) = " + result1)
        
        var result2 = greet("Alice")
        showAlert("greet result: " + result2)
        
        var result3 = factorial(5)
        showAlert("factorial(5) = " + result3)
        
        var result4 = fibonacci(6)
        showAlert("fibonacci(6) = " + result4)
        
        var result5 = countDown(7)
        showAlert("countDown result: " + result5)
        
        /* Ursprüngliche Tests */
        var a = 4
        var b = 2
        /* Berechnung von c */ var c = a * b * 4 / 2
        var d = a / b
        var flag = false
        var neg = -a
        var pos = +a
        var inv = !flag
        
        showAlert("c = " + c + ", d = " + d)
        showAlert("neg = " + neg + ", pos = " + pos + ", inv = " + inv)

        /* Teste mathematische Operationen */
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
        
        /* 
         * Neue for-Schleife Tests
         * Diese können auch verschachtelte Kommentare enthalten
         */
        showAlert("Testing for-loop:")
        for (var i = 0; i < 5; i++) {
            showAlert("For loop i = " + i)
        }
        
        /* for-Schleife mit externen Variablen */
        showAlert("For loop with external variables:")
        var j = 10
        for (; j > 5; j = j - 1) {
            showAlert("For loop j = " + j)
        }
        
        /* for-Schleife ohne Initialisierung */
        showAlert("For loop starting from existing variable:")
        var k = 0
        for (; k <= 2; k++) {
            showAlert("For loop k = " + k)
        }
        
        /* for-Schleife mit Dekrement */
        showAlert("For loop with decrement:")
        for (var m = 5; m > 0; m--) {
            showAlert("For loop m = " + m)
        }
        
        /*
         * Break und Continue Tests
         * Diese testen die Kontrollfluss-Mechanismen
         */
        showAlert("Testing break and continue:")
        
        /* Break Test */
        showAlert("Break test - should stop at 3:")
        for (var n = 0; n < 10; n++) {
            if (n == 3) {
                break
            }
            showAlert("Break test n = " + n)
        }
        
        /* Continue Test */
        showAlert("Continue test - should skip even numbers:")
        for (var p = 0; p < 6; p++) {
            if (p == 2 || p == 4) {
                continue
            }
            showAlert("Continue test p = " + p)
        }
        
        /* While mit break/continue */
        showAlert("While loop with break and continue:")
        var q = 0
        while (q < 10) {
            q++
            if (q == 3 || q == 7) {
                continue
            }
            if (q == 8) {
                break
            }
            showAlert("While q = " + q)
        }
        /* Data class tests */
        data class Person(name, age) // CHG
        var user = Person("Alice", 30)
        showAlert("Person name = " + user.name)
        showAlert("Person name = ${'$'}{user.name}") // CHG: interpolation demo
        showAlert("Person age = " + user.age)
        user.age = user.age + 1
        showAlert("Person age after birthday = " + user.age)
        showAlert("Next year age = ${'$'}{user.age + 1}") // CHG: expression interpolation
        /* Ende des Testcodes */
    """.trimIndent()

    try {
        val ast = MiniLanguageGrammar.parseToEnd(code)
        val interpreter = Interpreter()

        println("Parsing erfolgreich. AST:")
        ast.forEach { println("  $it") }
        println("\nAusführung:")

        interpreter.interpret(ast)

        println("\nVariablen nach Ausführung:")
        println("result1 = ${interpreter.getVariable("result1")}")
        println("result2 = ${interpreter.getVariable("result2")}")
        println("result3 = ${interpreter.getVariable("result3")}")
        println("result4 = ${interpreter.getVariable("result4")}")
        println("result5 = ${interpreter.getVariable("result5")}")
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
        println("m = ${interpreter.getVariable("m")}")
        println("n = ${interpreter.getVariable("n")}")
        println("p = ${interpreter.getVariable("p")}")
        println("q = ${interpreter.getVariable("q")}")
        println("name = ${interpreter.getVariable("name")}")

    } catch (e: Exception) {
        println("Fehler beim Parsen oder Ausführen: ${e.message}")
        e.printStackTrace()
    }
}

// TODO:
// - Blöcke mit deren Scopes {}
// - Fehlerbehandlung mit Zeilennummer, Spalte
// - Compound Assignment Operatoren (+=, -=, *=, /=)
// - Prefix Increment/Decrement (++i, --i)
// - Modulo-Operator (%)
// - Float/Double Unterstützung