package taller
import org.scalatest.funsuite.AnyFunSuite

class TestConjuntoDifusos extends AnyFunSuite {
    val c = new ConjDifusos()

    test("Pertenece 3 en muchoMayorQue(3, 7)"){
        assert(c.Pertenece(3, c.muchoMayorQue(3, 7)) == 0.0)
    }

  
}
