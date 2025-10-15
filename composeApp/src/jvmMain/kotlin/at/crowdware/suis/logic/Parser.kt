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

data class ForStatement(
    val init: Statement?,
    val condition: Expression?,
    val update: Statement?,
    val body: List<Statement>
) : Statement()

data class ForInStatement(
    val variable: String,
    val iterable: Expression,
    val body: List<Statement>
) : Statement()

data class BreakStatement(val dummy: Unit = Unit) : Statement()
data class ContinueStatement(val dummy: Unit = Unit) : Statement()
data class ReturnStatement(val value: Expression?) : Statement()
data class DataClassDeclaration(
    val name: String,
    val fields: List<String>
) : Statement()

data class FunctionDeclaration(
    val name: String,
    val parameters: List<String>,
    val body: List<Statement>
) : Statement()

class BreakException : Exception()
class ContinueException : Exception()
class ReturnException(val value: Any?) : Exception()

sealed class Expression : ASTNode()
data class UnaryExpression(val operator: String, val expr: Expression) : Expression()
data class BinaryExpression(val left: Expression, val operator: String, val right: Expression) : Expression()
data class FunctionCall(val name: String, val arguments: List<Expression>) : Expression()
data class StringLiteral(val value: String) : Expression()
data class NumberLiteral(val value: Int) : Expression()
data class BooleanLiteral(val value: Boolean) : Expression()
data class Identifier(val name: String) : Expression()
data class MemberAccess(val receiver: Expression, val member: String) : Expression()
data class MethodCall(val receiver: Expression, val method: String, val arguments: List<Expression>) : Expression()
data class ArrayLiteral(val elements: List<Expression>) : Expression()

data class PostfixExpression(
    val expr: Expression,
    val operator: String
) : Expression()

object MiniLanguageGrammar : Grammar<List<Statement>>() {
    val WS by regexToken("\\s+", ignore = true)
    val LINE_COMMENT by regexToken("//[^\\r\\n]*", ignore = true)
    val BLOCK_COMMENT by regexToken("/\\*[\\s\\S]*?\\*/", ignore = true)

    val EQUALS by literalToken("==")
    val NOT_EQUALS by literalToken("!=")
    val LE by literalToken("<=")
    val GE by literalToken(">=")
    val PP by literalToken("++")
    val MM by literalToken("--")
    val AND by literalToken("&&")
    val OR by literalToken("||")
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
    val DATA by literalToken("data")
    val CLASS by literalToken("class")
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
    val DOT by literalToken(".")
    val SEMICOLON by literalToken(";")
    val LBRACKET by literalToken("[")
    val RBRACKET by literalToken("]")
    val STRING by regexToken("\"[^\"]*\"")
    val NUMBER by regexToken("\\d+")
    val IN by regexToken("\\bin\\b")
    val IDENTIFIER by regexToken("[a-zA-Z_][a-zA-Z0-9_]*")
    val expression: Parser<Expression> by parser { logicalExpression }
    val identifier by IDENTIFIER use { Identifier(text) }
    val stringLiteral by STRING use { StringLiteral(text.substring(1, text.length - 1)) }
    val numberLiteral by NUMBER use { NumberLiteral(text.toInt()) }
    val booleanLiteral by (TRUE or FALSE) use { BooleanLiteral(text.toBoolean()) }
    val parenthesizedExpression by (-LPAREN and expression and -RPAREN)
    val argumentList by separatedTerms(expression, COMMA, acceptZero = true)
    val functionCall by (IDENTIFIER and -LPAREN and argumentList and -RPAREN) use { FunctionCall(t1.text, t2) }
    val arrayLiteral by (-LBRACKET and argumentList and -RBRACKET) use { ArrayLiteral(this) }
    val primaryExpression by (
            functionCall or arrayLiteral or stringLiteral or numberLiteral or booleanLiteral or identifier or parenthesizedExpression
            )
    val postfixExpression: Parser<Expression> by parser {
        primaryExpression and optional(PP or MM)
    }.map { (expr, op) ->
        if (op != null) PostfixExpression(expr, op.text) else expr
    }
    val methodCall: Parser<Expression> = parser {
        (postfixExpression and -DOT and IDENTIFIER and -LPAREN and argumentList and -RPAREN).map { (expr, method, args) ->
            MethodCall(expr, method.text, args)
        }
    }
    
    val memberAccess: Parser<Expression> = parser {
        (postfixExpression and -DOT and IDENTIFIER).map { (expr, member) ->
            MemberAccess(expr, member.text)
        }
    }
    
    val memberExpression: Parser<Expression> = parser {
        methodCall or memberAccess or postfixExpression
    }

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

    val varDeclaration by (-VAR and IDENTIFIER and -ASSIGN and expression) use { VarDeclaration(t1.text, t2) }
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
    val blockStatements: Parser<List<Statement>> by parser { separatedTerms(statement, WS, acceptZero = true) }
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

    val forStatement: Parser<Statement> by parser {
        -FOR and -LPAREN and
                optional(varDeclaration or assignment) and -SEMICOLON and
                optional(expression) and -SEMICOLON and
                optional(assignment or expressionStatement) and -RPAREN and
                -LBRACE and blockStatements and -RBRACE
    }.map { (init, condition, update, body) ->
        ForStatement(init, condition, update, body)
    }

    val forInStatement: Parser<Statement> by parser {
        -FOR and -LPAREN and IDENTIFIER and -IN and expression and -RPAREN and
                -LBRACE and blockStatements and -RBRACE
    }.map { (variable, iterable, body) ->
        ForInStatement(variable.text, iterable, body)
    }

    val breakStatement by BREAK use { BreakStatement() }
    val continueStatement by CONTINUE use { ContinueStatement() }

    val returnStatement by (RETURN and optional(expression)) use { ReturnStatement(t2) }

    val parameterList by separatedTerms(IDENTIFIER, COMMA, acceptZero = true)
    val functionDeclaration: Parser<Statement> by (
        -FUN and IDENTIFIER and -LPAREN and parameterList and -RPAREN and -LBRACE and blockStatements and -RBRACE
    ) use {
        FunctionDeclaration(t1.text, t2.map { it.text }, t3)
    }

    val dataClassDeclaration: Parser<Statement> by (
        -DATA and -CLASS and IDENTIFIER and -LPAREN and parameterList and -RPAREN
    ) use {
        DataClassDeclaration(t1.text, t2.map { it.text })
    }

    val statement: Parser<Statement> by functionDeclaration or dataClassDeclaration or varDeclaration or 
        ifStatement or whileStatement or forInStatement or forStatement or
        breakStatement or continueStatement or returnStatement or 
        assignment or expressionStatement
    override val rootParser by oneOrMore(statement)
}

class Interpreter {
    private val variables = mutableMapOf<String, Any>()
    private val functions = mutableMapOf<String, FunctionDeclaration>()
    private val dataClasses = mutableMapOf<String, DataClassDeclaration>()

    data class DataInstance(val className: String, val fields: MutableMap<String, Any?>) {
        override fun toString(): String = "${'$'}className${'$'}fields"
    }

    init {
        variables["name"] = "Art"
    }

    fun interpret(statements: List<Statement>) {
        statements.forEach { statement ->
            when (statement) {
                is FunctionDeclaration -> functions[statement.name] = statement
                is DataClassDeclaration -> dataClasses[statement.name] = statement
                else -> {}
            }
        }

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
                            continue
                        }
                    }
                } catch (e: BreakException) {
                }
            }
            is ForStatement -> {
                try {
                    statement.init?.let { executeStatement(it) }

                    while (true) {
                        if (statement.condition != null) {
                            val conditionResult = evaluateExpression(statement.condition)
                            if (conditionResult != true) break
                        }

                        try {
                            statement.body.forEach { executeStatement(it) }
                        } catch (e: ContinueException) {
                        }

                        // Update-Statement ausführen
                        statement.update?.let { executeStatement(it) }
                    }
                } catch (e: BreakException) {
                }
            }
            is ForInStatement -> {
                try {
                    val iterable = evaluateExpression(statement.iterable)
                    val array = iterable as? MutableList<*> ?: error("for-in requires an array")
                    
                    for (element in array) {
                        variables[statement.variable] = element ?: Unit
                        try {
                            statement.body.forEach { executeStatement(it) }
                        } catch (e: ContinueException) {
                            continue
                        }
                    }
                } catch (e: BreakException) {
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

            }
            is DataClassDeclaration -> {

            }
            is ExpressionStatement -> {
                evaluateExpression(statement.expression)
            }
        }
    }

    private fun evaluateExpression(expression: Expression): Any? {
        return when (expression) {
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
            is MemberAccess -> {
                val recv = evaluateExpression(expression.receiver)
                when (recv) {
                    is DataInstance -> recv.fields[expression.member]
                    is MutableList<*> -> {
                        // Handle array method calls without parentheses
                        when (expression.member) {
                            "size" -> recv.size
                            else -> error("Unknown array property: ${expression.member}")
                        }
                    }
                    else -> error("Member access on non-object/array")
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
            is MethodCall -> {
                val receiver = evaluateExpression(expression.receiver)
                val args = expression.arguments.map { evaluateExpression(it) }
                executeArrayMethod(receiver, expression.method, args)
            }
            is ArrayLiteral -> {
                val elements = expression.elements.map { evaluateExpression(it) }
                elements.toMutableList()
            }
            is StringLiteral  -> {
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

    private fun evaluateTemplateExpr(text: String): Any? {
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
                    val start = ++i
                    while (i < src.length && src[i] != '"') i++
                    val lit = src.substring(start, i)
                    if (i < src.length && src[i] == '"') i++
                    toks += Tok("STR", lit)
                }
                c == '.' -> { toks += Tok("DOT", "."); i++ }
                c == ',' -> { toks += Tok("COMMA", ","); i++ }
                c == '+' -> { toks += Tok("PLUS", "+"); i++ }
                c == '-' -> { toks += Tok("MINUS", "-"); i++ }
                c == '*' -> { toks += Tok("MUL", "*"); i++ }
                c == '/' -> { toks += Tok("DIV", "/"); i++ }
                c == '(' -> { toks += Tok("LP", "("); i++ }
                c == ')' -> { toks += Tok("RP", ")"); i++ }
                else -> {
                    return src
                }
            }
        }
        var p = 0
        fun peek(t: String) = p < toks.size && toks[p].type == t
        fun eat(t: String): Tok { val tok = toks.getOrNull(p); require(tok != null && tok.type == t) {"Expected ${'$'}t"}; p++; return tok }

        fun callMethod(receiver: Any?, name: String, args: List<Any?>): Any? {
            if (receiver == null) return null
            val cls = receiver.javaClass

            val candidateNames = sequence {
                yield(name)
                if (name.equals("toUpper", true)) { yield("toUpperCase") ; yield("uppercase") }
                if (name.equals("toLower", true)) { yield("toLowerCase") ; yield("lowercase") }
            }.toList()
            val methods = cls.methods.filter { m -> candidateNames.any { it.equals(m.name, ignoreCase = false) } && m.parameterCount == args.size }
            val m = methods.firstOrNull()
            return try {
                m?.invoke(receiver, *args.toTypedArray())
            } catch (e: Exception) {
                null
            }
        }

        fun parseExpr(): Any? {
            fun parseTerm(): Any? {
                fun parseFactor(): Any? {
                    when {
                        peek("NUM") -> return eat("NUM").s.toInt()
                        peek("STR") -> return eat("STR").s
                        peek("LP") -> { eat("LP"); val v = parseExpr(); eat("RP"); return v }
                        peek("ID") -> {
                            var cur: Any? = variables[eat("ID").s]
                            loop@ while (peek("DOT")) {
                                eat("DOT")
                                val member = eat("ID").s
                                if (peek("LP")) {
                                    eat("LP")
                                    val args = mutableListOf<Any?>()
                                    if (!peek("RP")) {
                                        args += parseExpr()
                                        while (peek("COMMA")) { eat("COMMA"); args += parseExpr() }
                                    }
                                    eat("RP")
                                    cur = callMethod(cur, member, args)
                                } else {
                                    cur = when (cur) {
                                        is DataInstance -> cur.fields[member]
                                        else -> null
                                    }
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
                        left is String || right is String -> "${'$'}left${'$'}right"
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
            "println"   -> { println(args.firstOrNull()?.toString() ?: ""); Unit }
            else -> {
                dataClasses[name]?.let { dc ->
                    if (args.size != dc.fields.size) error("Data class '${'$'}name' expects ${'$'}{dc.fields.size} args, got ${'$'}{args.size}")
                    val fieldMap = mutableMapOf<String, Any?>()
                    dc.fields.forEachIndexed { idx, f -> fieldMap[f] = args[idx] }
                    return DataInstance(dc.name, fieldMap)
                }
                val function = functions[name] ?: error("Unknown function: ${'$'}name")

                if (args.size != function.parameters.size) {
                    error("Function '${'$'}name' expects ${'$'}{function.parameters.size} arguments, got ${'$'}{args.size}")
                }

                val savedVariables = variables.toMap()

                try {
                    function.parameters.forEachIndexed { index, param ->
                        variables[param] = args[index] ?: Unit
                    }

                    function.body.forEach { executeStatement(it) }

                    null

                } catch (e: ReturnException) {
                    e.value
                } finally {
                    variables.clear()
                    variables.putAll(savedVariables)
                }
            }
        }
    }

    private fun executeArrayMethod(receiver: Any?, method: String, args: List<Any?>): Any? {
        val array = receiver as? MutableList<Any?> ?: error("Method '$method' can only be called on arrays")
        
        return when (method) {
            "add" -> {
                if (args.size != 1) error("add() expects exactly 1 argument")
                array.add(args[0])
                Unit
            }
            "removeAt" -> {
                if (args.size != 1) error("removeAt() expects exactly 1 argument")
                val index = args[0] as? Int ?: error("removeAt() index must be an integer")
                if (index < 0 || index >= array.size) error("Index $index out of bounds for array of size ${array.size}")
                array.removeAt(index)
            }
            "size" -> {
                if (args.isNotEmpty()) error("size() expects no arguments")
                array.size
            }
            "contains" -> {
                if (args.size != 1) error("contains() expects exactly 1 argument")
                array.contains(args[0])
            }
            "remove" -> {
                if (args.size != 1) error("remove() expects exactly 1 argument")
                array.remove(args[0])
            }
            else -> error("Unknown array method: $method")
        }
    }

    fun setVariable(name: String, value: Any) {
        variables[name] = value
    }

    fun getVariable(name: String): Any? = variables[name]
}

fun test() {
    // Test nur die Function-Deklaration
    val justFunctionCode = """
        fun inc(x) {
            return x + 1
        }
    """.trimIndent()

    try {
        println("Testing ONLY function declaration...")
        val ast = MiniLanguageGrammar.parseToEnd(justFunctionCode)
        println("Function declaration parses successfully! AST size: ${ast.size}")
        
    } catch (e: Exception) {
        println("Function declaration error: ${e.message}")
        e.printStackTrace()
    }

    // Test nur Function-Call
    val justCallCode = """
        var y = inc(41)
    """.trimIndent()

    try {
        println("Testing ONLY function call...")
        val ast = MiniLanguageGrammar.parseToEnd(justCallCode)
        println("Function call parses successfully! AST size: ${ast.size}")
        
    } catch (e: Exception) {
        println("Function call error: ${e.message}")
        e.printStackTrace()
    }

    // Complete integration test - all features including arrays
    val completeCode = """
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
        
        /* Teste benutzerdefinierte Funktionen */
        var result1 = add(5, 3)
        showAlert("add(5, 3) = " + result1)
        
        var result2 = greet("Alice")
        showAlert("greet result: " + result2)
        
        /* Ursprüngliche Tests */
        var a = 4
        var b = 2
        var c = a * b * 4 / 2
        var d = a / b
        var flag = false
        
        showAlert("c = " + c + ", d = " + d)

        /* Teste mathematische Operationen */
        if (c == 8 && d == 2) {
            showAlert("Multiplikation und Division funktionieren!")
        }
        
        /* Array Tests - Testing all required functionality */
        
        // Test empty array creation
        var array = []
        println("Empty array created: " + array.size)
        
        // Test array with string elements
        var names = ["Alex", "Lea", "Tom"]
        println("Names array size: " + names.size)
        
        // Test array with number elements
        var numbers = [1, 1, 2, 3, 5]
        println("Numbers array size: " + numbers.size)
        
        // Test for-in loop with arrays
        println("Iterating over names:")
        for (name in names) {
            println(name)
        }
        
        println("Iterating over numbers:")
        for (number in numbers) {
            println(number)
        }
        
        // Test array methods
        println("Testing array methods:")
        
        // Add method
        numbers.add(8)
        println("After adding 8, numbers size: " + numbers.size)
        
        // Contains method
        var contains3 = numbers.contains(3)
        println("Numbers contains 3: " + contains3)
        
        var contains10 = numbers.contains(10)
        println("Numbers contains 10: " + contains10)
        
        // RemoveAt method
        numbers.removeAt(0)
        println("After removing first element, numbers size: " + numbers.size)
        
        // Size method (called as function)
        var currentSize = numbers.size()
        println("Current numbers size (method call): " + currentSize)
        
        // Remove method
        var removedLea = names.remove("Lea")
        println("Removed Lea: " + removedLea)
        println("Names after removing Lea:")
        for (name in names) {
            println(name)
        }
        
        // Test property access vs method call
        println("Size as property: " + names.size)
        println("Size as method: " + names.size())
        
        println("Array tests completed!")
    """.trimIndent()

    try {
        println("\nTesting complete integrated code...")
        val ast = MiniLanguageGrammar.parseToEnd(completeCode)
        println("Complete code parses successfully!")
        
        val interpreter = Interpreter()
        println("\n=== VOLLSTÄNDIGE AUSFÜHRUNG ===")
        interpreter.interpret(ast)
        println("\n=== AUSFÜHRUNG BEENDET ===")
        
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
