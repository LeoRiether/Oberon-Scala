package br.unb.cic.oberon.stdlib

import br.unb.cic.oberon.ast._
import br.unb.cic.oberon.util.Values

object Functions {
    def abs(x: Int): Int = Math.abs(x)
    def odd(x: Int): Boolean =
        if (x % 2 == 1)
            true
        else
            false
}

object StandardLibrary {
    def abs = Procedure(
        "ABS", // name
        List(FormalArg("x", IntegerType)), // arguments
        Some(IntegerType), // return type
        List(),
        List(),

        ScalaStmt(env => {
            val value = env.lookup("x") match {
                case Some(IntValue(x)) => IntValue(Functions.abs(x))
                case _ => ???
            }
            env.setLocalVariable(Values.ReturnKeyWord, value)
        })
    )

    def odd = Procedure(
        "ODD", // name
        List(FormalArg("x", IntegerType)), // arguments
        Some(BooleanType), // return type
        List(),
        List(),

        ScalaStmt(env => {
            val value = env.lookup("x") match {
                case Some(IntValue(x)) => BoolValue(Functions.odd(x))
                case _ => ???
            }
            env.setLocalVariable(Values.ReturnKeyWord, value)
        })
    )

    val procedures = Map(
        abs.name -> abs,
        odd.name -> odd
    )
}