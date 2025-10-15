package at.crowdware.suis

import at.crowdware.suis.logic.DataClassDeclaration
import at.crowdware.suis.logic.MiniLanguageGrammar
import at.crowdware.suis.logic.NumberLiteral
import at.crowdware.suis.logic.StringLiteral
import at.crowdware.suis.logic.VarDeclaration
import com.github.h0tk3y.betterParse.grammar.parseToEnd
import com.github.h0tk3y.betterParse.parser.ParseException
import kotlin.test.assertTrue
import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertFailsWith
import at.crowdware.suis.logic.MemberAssignment
import at.crowdware.suis.logic.MemberAccess
import at.crowdware.suis.logic.FunctionDeclaration
import at.crowdware.suis.logic.WhileStatement

class ComposeAppDesktopTest {

    @Test
    fun testIfElseBracketsParsing() {
        val code = """
            if (name == "Art") {
                submit()
            } else {
                showAlert("?")
            }
        """.trimIndent()

        val result = MiniLanguageGrammar.parseToEnd(code)
        assertEquals(1, result.size)

        // CHG: detailed AST checks via reflection (robust across class/field names)
        val stmt = result[0]
        // class name should indicate an if-statement of some sort
        assertTrue(stmt.javaClass.simpleName.contains("If", ignoreCase = true))

        // then/else blocks should each contain exactly one statement
        val thenCount = getBlockCount(stmt, preferNames = listOf("then", "thenBlock", "thenBranch"))
        val elseCount = getBlockCount(stmt, preferNames = listOf("else", "elseBlock", "elseBranch"))
        assertEquals(1, thenCount)
        assertEquals(1, elseCount)
    }

    @Test
    fun testIfElseParsing() {
        val code = """
            if name == "Art" {
                submit()
            } else {
                showAlert("?")
            }
        """.trimIndent()

        val result = MiniLanguageGrammar.parseToEnd(code)
        assertEquals(1, result.size)

        // CHG: detailed AST checks via reflection (robust across class/field names)
        val stmt = result[0]
        // class name should indicate an if-statement of some sort
        assertTrue(stmt.javaClass.simpleName.contains("If", ignoreCase = true))

        // then/else blocks should each contain exactly one statement
        val thenCount = getBlockCount(stmt, preferNames = listOf("then", "thenBlock", "thenBranch"))
        val elseCount = getBlockCount(stmt, preferNames = listOf("else", "elseBlock", "elseBranch"))
        assertEquals(1, thenCount)
        assertEquals(1, elseCount)
    }

    private fun getBlockCount(stmt: Any, preferNames: List<String>): Int {
        val cls = stmt.javaClass
        // Look for a List-like field whose name contains any of the preferred keywords
        val fields = cls.declaredFields
        val field = fields.firstOrNull { f ->
            preferNames.any { key -> f.name.contains(key, ignoreCase = true) } &&
                    (List::class.java.isAssignableFrom(f.type) || f.type.name.contains("List"))
        } ?: return -1
        field.isAccessible = true
        val v = field.get(stmt)
        return (v as? List<*>)?.size ?: -1
    }

    @Test
    fun testDataClassParsing() {
        val code = """
            data class Person(name, age)
            var p = Person("Alice", 30)
        """.trimIndent()

        val result = MiniLanguageGrammar.parseToEnd(code)
        assertEquals(2, result.size)
        val decl = result[0]
        assertTrue(decl is DataClassDeclaration)
        val dataDecl = decl as DataClassDeclaration // CHG
        assertEquals("Person", dataDecl.name)
        assertEquals(listOf("name", "age"), dataDecl.fields)
    }

    @Test
    fun testDataClassParsingDetailed() {
        val code = """
            data class Person(name, age)
            var p = Person("Alice", 30)
        """.trimIndent()

        val result = MiniLanguageGrammar.parseToEnd(code)
        assertEquals(2, result.size)
        val decl = result[0]
        assertTrue(decl is DataClassDeclaration)
        val dataDecl = decl as DataClassDeclaration // CHG: cast for property access
        assertEquals("Person", dataDecl.name)
        assertEquals(listOf("name", "age"), dataDecl.fields)

        val v = result[1]
        assertTrue(v is VarDeclaration)
        assertEquals("p", v.name)
        // value is a call expression in current grammar; we only assert it's not a literal
        assertTrue(v.value !is StringLiteral && v.value !is NumberLiteral)
    }

    @Test
    fun testMemberAccessAndAssignmentParsing() {
        val code = """
            data class Person(name, age)
            var p = Person("Bob", 18)
            p.age = p.age + 1
        """.trimIndent()

        try {
            val result = MiniLanguageGrammar.parseToEnd(code)
            assertEquals(3, result.size)
        } catch (e: ParseException) {
            println("Parse error in testMemberAccessAndAssignmentParsing: ${e.message}")
            throw e
        }
    }

    @Test
    fun testMemberAssignmentDetailed() {
        val code = """
            data class Person(name, age)
            var p = Person("Bob", 18)
            p.age = p.age + 1
        """.trimIndent()
        val result = MiniLanguageGrammar.parseToEnd(code)
        assertEquals(3, result.size)
        val assign = result[2]
        assertTrue(assign is MemberAssignment)
        val mAssign = assign as MemberAssignment // CHG
        val target = mAssign.target
        assertTrue(target is MemberAccess)
        val mAccess = target as MemberAccess // CHG
        assertEquals("age", mAccess.member)
    }

    @Test
    fun testFunctionDeclAndCallParsing() {
        val code = """
            fun inc(x) {
                return x + 1
            }
            var y = inc(41)
        """.trimIndent()

        val result = MiniLanguageGrammar.parseToEnd(code)
        assertEquals(2, result.size)
    }

    @Test
    fun testFunctionDeclDetailed() {
        val code = """
            fun inc(x) {
                return x + 1
            }
            var y = inc(41)
        """.trimIndent()
        val result = MiniLanguageGrammar.parseToEnd(code)
        assertEquals(2, result.size)
        val fn = result[0]
        assertTrue(fn is FunctionDeclaration)
        val fDecl = fn as FunctionDeclaration // CHG
        assertEquals("inc", fDecl.name)
        val paramNames = getFunctionParamNames(fDecl) // CHG: robust across field names
        assertEquals(listOf("x"), paramNames)
    }

    @Test
    fun testStringInterpolationAccepted() {
        val code = """
            data class Person(name, age)
            var user = Person("Art", 42)
            showAlert("Name: ${'$'}{user.name}, Next: ${'$'}{user.age + 1}")
        """.trimIndent()

        val result = MiniLanguageGrammar.parseToEnd(code)
        assertEquals(3, result.size)
    }

    @Test
    fun testWhileAndBreakContinueParsing() {
        val code = """
            var i = 0
            while (i < 10) {
                i = i + 1
                if (i == 3) { continue }
                if (i == 8) { break }
            }
        """.trimIndent()

        val result = MiniLanguageGrammar.parseToEnd(code)
        assertEquals(2, result.size)
    }

    @Test
    fun testWhileParsingDetailed() {
        val code = """
            var i = 0
            while (i < 3) {
                i = i + 1
            }
        """.trimIndent()
        val result = MiniLanguageGrammar.parseToEnd(code)
        assertEquals(2, result.size)
        val whileStmt = result[1]
        assertTrue(whileStmt is WhileStatement)
    }

    @Test
    fun testInvalidSyntaxThrows() {
        val bad = """
            data class X(a, b)
            var v = X(1 2)
        """.trimIndent()

        assertFailsWith<ParseException> {
            MiniLanguageGrammar.parseToEnd(bad)
        }
    }

    @Test
    fun testDeepMemberAccessDisallowedCurrently() {
        val code = """
            var a = obj.part.sub
        """.trimIndent()
        assertFailsWith<ParseException> {
            MiniLanguageGrammar.parseToEnd(code)
        }
    }

    // Array Tests
    @Test
    fun testEmptyArrayParsing() {
        val code = "var array = []"
        val result = MiniLanguageGrammar.parseToEnd(code)
        assertEquals(1, result.size)
        assertTrue(result[0] is VarDeclaration)
    }

    @Test
    fun testArrayLiteralParsing() {
        val code = """
            var names = ["Alex", "Lea", "Tom"]
            var numbers = [1, 2, 3, 5]
        """.trimIndent()
        val result = MiniLanguageGrammar.parseToEnd(code)
        assertEquals(2, result.size)
        assertTrue(result[0] is VarDeclaration)
        assertTrue(result[1] is VarDeclaration)
    }

    @Test
    fun testForInLoopParsing() {
        val code = """
            var names = ["Alex", "Lea"]
            for (name in names) {
                println(name)
            }
        """.trimIndent()
        val result = MiniLanguageGrammar.parseToEnd(code)
        assertEquals(2, result.size)
        assertTrue(result[0] is VarDeclaration)
        // Check that the second statement is a for-in loop
        val forInStmt = result[1]
        assertTrue(forInStmt.javaClass.simpleName.contains("ForIn", ignoreCase = true))
    }

    @Test
    fun testArrayMethodCallsParsing() {
        val code = """
            var numbers = [1, 2, 3]
            numbers.add(4)
            numbers.removeAt(0)
            var size = numbers.size()
            var contains = numbers.contains(2)
            numbers.remove(3)
        """.trimIndent()
        val result = MiniLanguageGrammar.parseToEnd(code)
        assertEquals(6, result.size)
        // Should all parse without errors
    }

    @Test
    fun testArrayPropertyAccess() {
        val code = """
            var names = ["Alex", "Lea"]
            var size = names.size
        """.trimIndent()
        val result = MiniLanguageGrammar.parseToEnd(code)
        assertEquals(2, result.size)
        assertTrue(result[0] is VarDeclaration)
        assertTrue(result[1] is VarDeclaration)
    }

    @Test
    fun testComplexArrayOperations() {
        val code = """
            var numbers = [1, 1, 2, 3, 5]
            for (number in numbers) {
                println(number)
            }
            numbers.add(8)
            var contains3 = numbers.contains(3)
            numbers.removeAt(0)
            var currentSize = numbers.size()
        """.trimIndent()
        val result = MiniLanguageGrammar.parseToEnd(code)
        assertEquals(6, result.size)
    }

    private fun getFunctionParamNames(fn: FunctionDeclaration): List<String> {
        val cls = fn.javaClass
        val candidates = listOf("params", "parameters", "paramNames", "args", "arguments")
        for (name in candidates) {
            val field = cls.declaredFields.firstOrNull { it.name == name } ?: continue
            field.isAccessible = true
            val v = field.get(fn)
            if (v is List<*>) return v.filterIsInstance<String>()
        }
        throw AssertionError("Could not locate parameter list on FunctionDeclaration. Fields=" + cls.declaredFields.joinToString { it.name })
    }
}
