package at.crowdware.suis

import at.crowdware.suis.logic.BinaryExpr
import at.crowdware.suis.logic.CallExpr
import at.crowdware.suis.logic.IfElseExpr
import at.crowdware.suis.logic.SimpleKotlinGrammar
import com.github.h0tk3y.betterParse.grammar.parseToEnd
import junit.framework.TestCase.assertTrue
import kotlin.test.Test
import kotlin.test.assertEquals

class ComposeAppDesktopTest {

    @Test
    fun example() {
        assertEquals(3, 1 + 2)
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

        val result = SimpleKotlinGrammar.parseToEnd(code)

        // Prüfen, ob Ergebnis eine Liste mit einem IfElseExpr ist
        assertEquals(1, result.size)
        val node = result[0]
        assertTrue(node is IfElseExpr)

        val ifNode = node as IfElseExpr

        // Optional: weitere Strukturprüfungen
        assertTrue(ifNode.condition is BinaryExpr)
        assertTrue(ifNode.thenBlock.statements.first() is CallExpr)
        assertTrue(ifNode.elseBlock?.statements?.first() is CallExpr)

        val thenCall = ifNode.thenBlock.statements.first() as CallExpr
        assertEquals("submit", thenCall.name)

        val elseCall = ifNode.elseBlock?.statements?.first() as CallExpr
        assertEquals("showAlert", elseCall.name)
    }
}