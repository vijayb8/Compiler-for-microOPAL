
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

/** Parser for Î¼-Opal-Compiler*/

object Parser {
  import de.tuberlin.uebb.comp1.moc.{ AbstractSyntax => AS }
  import AbstractSyntax._
  
  import de.tuberlin.uebb.comp1.moc.Token
  
  private type Toks = List[Token]
  /**
   * Starts the parser
   *
   * @param inp the token sequence (result of scanner)
   * @param opts [[Options]] given as arguments to comiler
   * @return either an error message [[Diag]] or a list of definitions [[Def]]
   */
  def parse(s: Toks, opts: Options): Either[Diag, Prog] = {
   parseProg(s)._1    
  }
  
  
   private def parseProg(s: Toks): (Either[Diag, Prog], Toks) = {
     val ft=s.head
    val (d1, s1)  = parseDef(s)
    if (d1.isLeft){
      (Left(d1.left.get), s1)
    }
    else {
      val (ds2, s2) = parseListDef(d1.right.get,s1)
      if (ds2.isLeft) {
        (Left(ds2.left.get), s2)
      }
      else {
        val (t3, s3)  = parseEOF(s2)
        if (t3.isLeft) {
          (Left(t3.left.get), s3)
        }
        else {
          println("******************PARSER SUCCESS******************")
          (Right(AS.Prog(ds2.right.get,ft.getPosition)), s3)
        }    
      }    
    }    
  }
   
   private def parseEOF(s: Toks): (Either[Diag, Token], Toks) = {
    val ft = s.head
    if (ft == EofT()) {
      (Right(ft), s.tail)
    }else{
     // println(ft.getPosition)
      (Left(Diag("End of file expected " , ft.getPosition)), s)    
    }
  }
   
   private def parseComma(s: Toks): (Token, Toks) = {
     (s.head, s.tail)
  }
 
   private def parseDEF(s: Toks): (Token, Toks) = {
     (s.head, s.tail)
  }
   
   private def parseEQ(s: Toks): (Either[Diag, Token], Toks) = {
    val ft = s.head
    if (ft == DefAsT()) {
      (Right(ft), s.tail)
    } else 
    {
        (Left(Diag("== expected " , ft.getPosition)), s)
    }
  }
   
   private def parseMAIN(s: Toks): (Token, Toks) = {
     (s.head, s.tail)
  }
   
   private def parseCol(s: Toks): (Either[Diag, Token], Toks) = {
    val ft = s.head
    if (ft == ColonT()) {
      (Right(ft), s.tail)
    } else 
      (Left(Diag("Colon expected " , ft.getPosition)), s)
  }
   
   private def parseNat(s: Toks): (Token, Toks) = {
     (s.head, s.tail)
  }
   
   private def parseBool(s: Toks): (Either[Diag, Token], Toks) = {
    val ft = s.head
    if (ft == BoolT()) {
      (Right(ft), s.tail)
    } else 
        (Left(Diag("Boolean value expected  " , ft.getPosition)), s)
  }
   
   private def parseID(s: Toks): (Token, Toks) = {
     (s.head, s.tail)
  }
   
   private def parseOpen(s: Toks): (Either[Diag, Token], Toks) = {
    val ft = s.head
    if (ft == OpenT()) {
      (Right(ft), s.tail)
    } else 
      (Left(Diag("( expected " , ft.getPosition)), s)
  }
   
   private def parseClose(s: Toks): (Either[Diag, Token], Toks) = {
    val ft = s.head
    if (ft == CloseT()) {
      (Right(ft), s.tail)
    } else 
      (Left(Diag(") expected " , ft.getPosition)), s)
  }
   
   private def parseNumber(s: Toks): (Token, Toks) = {
    (s.head, s.tail)
  }
   
   private def parseTrue(s: Toks): (Token, Toks) = {
     (s.head, s.tail)
  }
   
   private def parseFalse(s: Toks): (Token, Toks) = {
     (s.head, s.tail)
  }
   
   private def parseIF(s: Toks): (Token, Toks) = {
    (s.head, s.tail)
  }
   
   private def parseTHEN(s: Toks): (Either[Diag, Token], Toks) = {
    val ft = s.head
    if (ft == ThenT()) {
      (Right(ft), s.tail)
    } else 
      (Left(Diag("THEN identifier " , ft.getPosition)), s)
  }
   
   private def parseELSE (s: Toks): (Token, Toks) = {
     (s.head, s.tail)
  }
   
   private def parseFI(s: Toks): (Either[Diag, Token], Toks) = {
    val ft = s.head
    if (ft == FiT()) {
      (Right(ft), s.tail)
    } else 
      (Left(Diag("FI identifier expected " , ft.getPosition)), s)
  }
   
   private def parseDef(s: Toks): (Either[Diag, Def], Toks) = {
     val ft = s.head
       if (ft == DefT()) {
         val (t1, s1) = parseDEF(s)
         val (l2, s2) = parseLeft(s1)
         if(l2.isLeft){
           (Left(l2.left.get), s2)
         }
         else {
           val (t3, s3) = parseEQ(s2)
           if (t3.isLeft){
             (Left(t3.left.get), s3)
           }
           else {
             val (e4, s4) = parseExpr(s3)
             if (e4.isLeft){
               (Left(e4.left.get), s4)
             }
             else {
               (Right(AS.Defm(l2.right.get,e4.right.get,ft.getPosition)), s4)
             }             
           }            
         }        
    }
       else 
         (Left(Diag("DEF unique identifier expected " , ft.getPosition)), s)
   }
          
   private def parseExprN(e: List[Expr], s: Toks) : (Either[Diag, List[Expr]], Toks) = {
     val ft = s.head
     if (ft == CommaT()){
       val (t1, s1) = parseComma(s)
       val (e2, s2) = parseExpr(s1)
       if (e2.isLeft) {
         (Left(e2.left.get), s2)
       }
       else {
         val (e3, s3) = parseExprN(e2.right.get::e, s2)
         if (e3.isLeft) {
           (Left(e3.left.get), s3)
         }
         else {
           (Right(e3.right.get), s3)
         }         
         }       
       }    
     else if (ft == CloseT()){
       (Right(e), s)
     }
     else 
       (Left(Diag(", or ) is expected " , ft.getPosition)), s)
   }
   
   private def parseJExpr (s: Toks) : (Either[Diag, List[Expr]], Toks) = {
     val ft = s.head
     if (ft == CloseT()) {
       (Right(List()), s)
     }
     else if ((ft.isNum) | (ft == TrueT()) | (ft == FalseT()) | (ft.isVar) | (ft == IfT())) {
       val (e1, s1) = parseExpr(s)
       if (e1.isLeft) {
         (Left(e1.left.get), s1)
       }
       else {
         val(e2, s2) = parseExprN(List(e1.right.get), s1)
         if (e2.isLeft) {
           (Left(e2.left.get), s2)
         }
         else {
           (Right(e2.right.get), s2)
         }         
       }       
     }
     else
       (Left(Diag(" ) or valid expression expected " , ft.getPosition)), s)
   }
   
   private def parseExprF(s: Toks) : (Either[Diag, List[Expr]], Toks) = {
     val ft = s.head
     if(ft == OpenT()){
       val (t1, s1) = parseOpen(s)
       if (t1.isLeft){
         (Left(t1.left.get), s1)
       }
       else {
         val (e2, s2) = parseJExpr(s1)
         if (e2.isLeft){
           (Left(e2.left.get), s2)
         }
         else {
           val (t3, s3) = parseClose(s2)
           if(t3.isLeft){
             (Left(t3.left.get), s3)
           }
           else {
               (Right(e2.right.get), s3)
             }          
           }           
         }                 
     }
     
     else if(ft == CommaT() | ft == ElseT() | ft == ThenT() | ft == FiT() | ft == CloseT() | ft == EofT() | ft == DefT()){
       (Right(List()), s)
     }
     else 
       (Left(Diag("Termination of the expression is improper " , ft.getPosition)), s)
   }
   private def parseExpr(s: Toks) : (Either[Diag, Expr], Toks) = {
     val ft = s.head
     if (ft.isNum){
       val (e1, s1) = parseNumber(s)
       (Right(AS.num(e1.toString(),ft.getPosition)), s1)
     }

     else if (ft == TrueT()){
       val (e1, s1) = parseTrue(s)
       (Right(AS.True("true",ft.getPosition)), s1)
     }
     else if (ft == FalseT()){
       val (e1, s1) = parseFalse(s)
       (Right(AS.False("false",ft.getPosition)), s1)
     }
     else if(ft.isVar){
       val (e1, s1) = parseID(s)
       val (e2, s2) = parseExprF(s1)
       if (e2.isLeft) {
         (Left(e2.left.get), s2)
       }
       else {
         (Right(AS.Expr_id(e1.toString(),e2.right.get,ft.getPosition)),s2)
       }       
     }
     else if(ft == IfT()){
       val (e1, s1) = parseIF(s)
       val (e2, s2) = parseExpr(s1)
       if (e2.isLeft) {
         (Left(e2.left.get) , s2)
       }
       else {
         val (e3, s3) = parseTHEN(s2)
         if (e3.isLeft) {
           (Left(e3.left.get), s3)
         }
         else {
           val (e4, s4) = parseExpr(s3)
           if (e4.isLeft) {
             (Left(e4.left.get), s4)
           }
           else {
             val (e5, s5) = parseP1(s4)
             if (e5.isLeft) {
               (Left(e5.left.get), s5)
             }
             else {
               val (e6, s6) = parseFI(s5)
               if (e6.isLeft) {
                 (Left(e6.left.get), s6)
               }
               else {
                 (Right(AS.condt(e2.right.get,e4.right.get,e5.right.get,ft.getPosition)), s6)
               }       
             }       
           }       
         }       
       }       
     }
     else sys.error ("wrong expression, You can consider adding a Number, Boolean, Function or a conditional expression " + ft.getPosition)
     
   }
   
   private def parseP1(s: Toks) : (Either[Diag, Expr], Toks) = {
     val ft = s.head
     if (ft == ElseT()){
       val (e1, s1) = parseELSE(s)
       val (e2, s2) = parseExpr(s1)
       if (e2.isLeft) {
         (Left(e2.left.get), s2)
       }
       else {
         (Right(e2.right.get), s2)
       }       
     }
     else if (ft == FiT()){
       (Right(AS.Expr_epsilon()), s)
     }
     else 
       (Left(Diag("ELSE or FI is missing in your conditional expr " , ft.getPosition)), s)
   }
   
   private def parseLeft(s: Toks) : (Either[Diag, Left], Toks) = {
     val ft = s.head
     if(ft == MainT()){
     val (t1, s1) = parseMAIN(s)
     val (t2, s2) = parseCol(s1)
     if (t2.isLeft)
       {((Left(t2.left.get)), s2)}
       else {
              val (t3, s3) = parseType(s2)
              if (t3.isLeft){
                ((Left(t3.left.get)), s3)
              }
              else {
                (Right(AS.Left_Main(t3.right.get,ft.getPosition)) , s3)
              }
            }
     }
     else if (ft.isVar) {
       val (t1, s1) = parseID(s)
       val (t2, s2) = parseOpen(s1)
       if (t2.isLeft) {
         (Left(t2.left.get), s2)
       }
       else {
         val (l3, s3) = parseL1(s2)
         if (l3.isLeft){
           (Left(l3.left.get), s3)
         }
         else {
           val (t4, s4) = parseClose(s3)
           if (t4.isLeft) {
             (Left(t4.left.get), s4)
           }
           else {
             val (t5, s5) = parseCol(s4)
             if (t5.isLeft) {
               (Left(t5.left.get), s5)
             }
             else {
               val (t6, s6) = parseType(s5)
               if (t6.isLeft){
                 (Left(t6.left.get), s6)
               }
               else {
                 (Right(AS.Left_id(t1.toString(),t6.right.get, l3.right.get,ft.getPosition)) , s6)
               }       
             }       
           }       
         }       
       }    
     }     
     else 
       (Left(Diag("Function name or MAIN unique identifier expected " , ft.getPosition)), s)
   }
   
   private def parseLeft2(s: Toks) : (Either[Diag, Left], Toks) = {
     val ft = s.head
     if(ft.isVar){
       val (t1, s1) = parseID(s)
       val (t2, s2) = parseCol(s1)
       if(t2.isLeft) {
         (Left(t2.left.get), s2)
       }
       else {
         val (t3, s3) = parseType(s2)
         if(t3.isLeft){
           (Left(t3.left.get), s3)
         }
         else {
           //println(s3.toString())
           (Right(AS.Left_2(t1.toString(), t3.right.get,ft.getPosition)), s3)
         }         
       }       
     }
     else 
       (Left(Diag("Function name expected " , ft.getPosition)), s)
   }
   
   private def parseLeftM (l:Left, s: Toks) : (Either[Diag, Left], Toks) = {
     val ft = s.head
     if (ft == CommaT()){
       val (t1, s1) = parseComma(s)
       val (l2, s2) = parseLeft2(s1)
       if (l2.isLeft){
         (Left(l2.left.get), s2)
       }
       else {
         val (l3, s3) = parseLeftM(l2.right.get,s2)
         if (l3.isLeft) {
           (Left(l3.left.get), s3)
         }
         else {
           //println("In Left m \n")
           (Right(AS.List_Left(l2.right.get,l3.right.get,ft.getPosition)),s3)
         }       
       }       
     }
     else if (ft == CloseT()) {
       (Right(AS.Left_1_epsilon()), s)
     }
     else 
       (Left(Diag(" , or ) are expected " , ft.getPosition)), s)
   } 
   
   
   private def parseL1(s: Toks) : (Either[Diag, Left], Toks) = {
     val ft = s.head
     if (ft.isVar){
       val (l1, s1) = parseLeft2(s)
       if (l1.isLeft) {
         (Left(l1.left.get), s1)
       }
       else {
         val (l2, s2) = parseLeftM(l1.right.get, s1)
         if (l2.isLeft) {
           (Left(l2.left.get), s2)
         }
         else {
           // println("In Left L1 \n")
           (Right(AS.List_Left(l1.right.get, l2.right.get,ft.getPosition)), s2)
         }       
       }       
     }
     else if (ft == CloseT()){
       (Right(AS.Left_1_epsilon()),s)
     }
     else 
       (Left(Diag(" ) or function name is expected " , ft.getPosition)), s)
   }
   
   private def parseType(s: Toks) : (Either[Diag, Type], Toks) = {
     val ft = s.head
     if(ft == NatT()){
       val (t1,s1) = parseNat(s)
       (Right(AS.TyNat),s1)
     }
     else if (ft == BoolT()){
       val (t1, s1) = parseBool(s)
       (Right(AS.TyBool),s1)
     }
     else
       (Left(Diag("A Type is expected " , ft.getPosition)), s)
   }
   
   private def parseListDef(d:Def, s:Toks) : (Either[Diag, Def], Toks) = {
     val ft = s.head
    if (ft == DefT()) {
     val (d1, s1)  = parseDef(s)
     if(d1.isLeft){
       (Left(d1.left.get), s1)
     }
     else {
       val (ds2, s2) = parseListDef(AS.List_Def(d,d1.right.get,ft.getPosition),s1)  
       if(ds2.isLeft){
         (Left(ds2.left.get), s2)
       }
       else {
         (Right(ds2.right.get), s2)
       }     
     }   
    }
    else if (ft == EofT()){
      (Right(d), s)
    }
    else 
      (Left(Diag("DEF unique identifier or EOF expected " , ft.getPosition)), s)
   }
  
}