/*
 * Copyright (c) 2013, TU Berlin
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *   * Redistributions of source code must retain the above copyright
 *     notice, this list of conditions and the following disclaimer.
 *   * Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimer in the
 *     documentation and/or other materials provided with the distribution.
 *   * Neither the name of the TU Berlin nor the
 *     names of its contributors may be used to endorse or promote products
 *     derived from this software without specific prior written permission.

 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS """AS IS""" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL TU Berlin BE LIABLE FOR ANY
 * DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 * */

package de.tuberlin.uebb.comp1.moc

/** The Î¼-Opal context checker */

object ContextChecker {
  import AbstractSyntax._
  import scala.collection.immutable.TreeMap
  import Ordering.Implicits._
  import de.tuberlin.uebb.comp1.moc.{ AbstractSyntax => AS }
  import scala.language.postfixOps
  import scala.collection.immutable.{ HashMap }
  //import scala.collection.mutable.Map
  /**
   * Starts context check for Î¼-Opal
   * @param prog Complete program [[Prog]]
   * @param opts [[Options]] given as arguments to compiler
   * @return A list of error messages
   */
  var tm:  Map[String,Type]=Map()
  var tm_args: Map[String, List[Type]] =Map()
  var tm_args_name: Map[String, List[String]] = Map()
  var error: Map[String, List[Diag]] = Map()
  var listUndefined: List[String] = List()
  var listTypeLeft: List[Type] = List()
  var listNameLeft: List[String] = List()
  val list_prim_func: List[String] = List("add", "sub", "div", "mul", "eq", "lt", "and", "or", "not")  
  
  
  private def Initialize_TypChecker() = {
     tm=Map()
     tm_args_name=Map()
     tm_args=Map()
     listUndefined=List()
     listNameLeft=List()
     listTypeLeft=List()
     error = Map()
  }

  def check(prog: Prog, opts: Options): Option[List[Diag]] = {
    Initialize_TypChecker()
    tm = Map("add" -> TyNat, "sub" -> TyNat, "mul" -> TyNat, "div" -> TyNat, "eq" -> TyBool, "lt" -> TyBool, "and" -> TyBool, "or" -> TyBool, "not" -> TyBool)
    tm_args= Map("add" -> List(TyNat, TyNat), "mul" -> List(TyNat, TyNat), "sub" -> List(TyNat, TyNat), "div" -> List(TyNat, TyNat), "eq" -> List(TyNat, TyNat), "lt" -> List(TyNat, TyNat), "and" -> List(TyBool, TyBool), "or" -> List(TyBool, TyBool), "not" -> List(TyBool))
    error+=("error"->List())
    startProg(prog, 1)
    startProg(prog, 2)
   // Initialize_TypChecker()
    if (error("error").isEmpty) {
      if (!tm.contains("MAIN")) {
        error += ("error" -> error("error").++:(List(Diag("Atleast one Main declaration should be present ", Global))))
        error.get("error")
      } else {
        None
      }
    } else {
      if (!tm.contains("MAIN")) {
        error += ("error" -> error("error").++:(List(Diag("Atleast one Main declaration should be present ", Global))))
      }
      error.get("error")
    }
  }
  private def startProg(prog: Prog, f: Int): Option[List[Diag]] = prog match {
    case Prog(d, p) => {
      val d1 = typeOfDef(d, f)
      d1
    }
    case _ => {
      error += ("error" -> error("error").++:(List(Diag("Unidentified beginning of the program ", Global))))
      error.get("error")
    }
  }
  private def typeOfType(t: Type) = t match {
    case TyNat => TyNat
    case TyBool => TyBool
    case TyUnknown => TyUnknown
  }

  private def typeOfLeftType(l: Left, id: String): (List[Type], List[String]) = l match {
    case List_Left(l1, l2, p) => {
      val (t1, s1) = typeOfLeftType(l1, id)
      val (t2, s2) = typeOfLeftType(l2, id)
      listTypeLeft ::: t1
      listTypeLeft ::: t2
      listNameLeft ::: s1 ::: s2
      (listTypeLeft, listNameLeft)
    }
    case Left_2(a, b, p) => {
      listTypeLeft ::= b
      listNameLeft ::= a
      (listTypeLeft, listNameLeft)
    }
    case Left_1_epsilon() => {
      (listTypeLeft, listNameLeft)
    }
  }
  private def typeOfLeft(l: Left, f: Int): (Type, String) = l match {
    case Left_Main(a, p) => {
      if (!tm.contains("MAIN")) {
        tm += ("MAIN" -> typeOfType(a))
        tm_args += ("MAIN" -> List())
        tm_args_name += ("MAIN" -> List())
        (a, "MAIN")
      } else {
        if (f == 1) {
          error += ("error" -> error("error").++:(List(Diag("More than one Main declaration", p))))
          (a, "MAIN")
        } else {
          (a, "MAIN")
        }
      }
    }
    case Left_2(a, b, p) => {
      (b, a)
    }
    case Left_id(a, b, c, p) => {
      if (tm.contains(a)) {
        if (f == 1) {
          error += ("error" -> error("error").++:(List(Diag("The Identifier " + a + " is already defined\n", p))))
          (b, a)
        } else {
          (b, a)
        }
      } else {
        tm += (a -> typeOfType(b))
        val (t1, s1) = typeOfLeftType(c, a)
        tm_args += (a -> t1)
        tm_args_name += (a -> s1)
        if (!(s1.length == s1.distinct.length)) {
          error += ("error" -> error("error").++:(List(Diag("The list of arguments for " + a + " need to be distinct\n", p))))
        }
        listTypeLeft = List()
        listNameLeft = List()
        (b, a)
      }
    }
    case Left_1_epsilon() => {
      (TyUnknown, "")
    }
    case List_Left(l1, l2, p) => {
      val (t1, a) = typeOfLeft(l1, f)
      val (t2, b) = typeOfLeft(l2, f)
      (TyUnknown, a)
    }

  }
  
  private def typeOfExpr(e: Expr, id: String, f: Int): Type = e match {
    case num(v, p) => {
      TyNat
    }
    case True(v, p) => {
      TyBool
    }
    case False(v, p) => {
      TyBool
    }
    case Expr_id(s, e, p) => {
      if (tm_args_name.contains(id)) {
        if (tm.contains(s)) {
          val listN = tm_args_name(id)
          if (listN.contains(s)) {
            val index: Int = listN.indexOf(s)
            val kt = tm_args(id)
            kt(index)
          } else {
            val t1 = tm_args(s)
            if (t1.length == e.length) {
              val y = for (ex <- e) {
                val e1 = typeOfExpr(ex, id, f)
                val index: Int = e.indexOf(ex)
                val t2 = t1(index)
                val ex_help = ex match {
                  case Expr_id(a, b, _) => (a)
                  case _ => ("Not an anrgument")
                }
                if (e1 == t2) {

                } else {
                  if(!(e1==TyUndef))
                  error += ("error" -> error("error").++:(List(Diag("Argument " + ex_help + " of function " + s + " expects " + get_type(t2) + " instead of " + get_type(e1) + "\n", p))))
                }

              }
              tm(s)
            } else {
              error += ("error" -> error("error").++:(List(Diag("The number of Arguments for identifier for " + s + " are not matching \n", p))))
              tm(s)
            }
          }
        } else {
          val listN = tm_args_name(id)
          if (listN.contains(s)) {
            if (e.length == 0) {
              val index: Int = listN.indexOf(s)
              val kt = tm_args(id)
              kt(index)
            } else {
              error += ("error" -> error("error").++:(List(Diag("The Identifier " + s + " is not defined before \n", p))))
              TyUnknown
            }
          } else {

            if (f == 2) {
              error += ("error" -> error("error").++:(List(Diag("The argument " + s + " has not been defined before \n", p))))
              TyUnknown
            } else {
              listUndefined ::= id
              TyUndef
            }
          }

        }
      } else {
        if (list_prim_func.contains(id)) {
          TyUnknown
        } else {
          error += ("error" -> error("error").++:(List(Diag("The id " + id + " is not present to collect the argument names \n", p))))
          TyUnknown
        }
      }
    }
    case Expr_epsilon() => {
      TyUnknown
    }
    case condt(e1, e2, e3, p) => {
      val t2 = typeOfExpr(e1, id, f)
      if (t2 == TyBool) {
        if (e3 != Expr_epsilon()) {
          val t3 = typeOfExpr(e2, id, f)
          val t4 = typeOfExpr(e3, id, f)
          if (t3 == t4) {
            t3
          } else {
            if (t3 == TyUndef || t4 == TyUndef) {
              TyUndef
            } else {
              error += ("error" -> error("error").++:(List(Diag("The Arguments between then and else expressions in conditional is not matching\n", p))))
              TyUnknown
            }
          }
        } else {
          val t3 = typeOfExpr(e2, id, f)
          t3
        }
      } else {
        error += ("error" -> error("error").++:(List(Diag("Expression e1 should be boolean \n", p))))
        TyUnknown
      }
    }
  }

  private def get_type(type_new: Type): String = type_new.toString() match {
    case "TyNat" => ("Nat")
    case "TyBool" => ("Boolean")
    case "TyUnknown" => ("Unknown")
    case "TyUndef" => ("Undefined")
  }

  def typeOfDef(d: Def, f: Int): Option[List[Diag]] = d match {
    case Defm(b, c, p) => {

      val (a1, a) = typeOfLeft(b, f)
      if (f == 1 || listUndefined.contains(a)) {
        val a2 = typeOfExpr(c, a, f)
        if (a1 == a2) {
          error.get("error")
        } else {
          if (a2 == TyUndef) {
            error.get("error")
          } else {
            error += ("error" -> error("error").++:(List(Diag("Found Expression type: " + get_type(a2) + ", Expected Expression type: " + get_type(a1) + "\n", p))))
            error.get("error")
          }
        }
      } else {
        error.get("error")
      }
    }
    case List_Def(d1, d2, p) => {
      typeOfDef(d1, f)
      typeOfDef(d2, f)
      None
    }
    case _ => Some(List(Diag("Context checker not yet implemented\n", Global)))
  }

}  