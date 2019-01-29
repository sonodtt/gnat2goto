with Nlists;                use Nlists;
with Sem_Aux;               use Sem_Aux;
with Snames;                use Snames;
with Stringt;               use Stringt;
with Treepr;                use Treepr;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with GOTO_Utils;            use GOTO_Utils;
with Uint_To_Binary;        use Uint_To_Binary;
with Stand;
with Binary_To_Hex;         use Binary_To_Hex;
with Ada.Exceptions;

with Urealp; use Urealp;
with Range_Check; use Range_Check;
with Ada.Text_IO;           use Ada.Text_IO;

with Sem_Util;              use Sem_Util;
with Stand;                 use Stand;
with Switch;                use Switch;
with Einfo;                 use Einfo;
with Atree;                 use Atree;
with Uintp;                 use Uintp;

with Follow;                use Follow;
with Ireps;                 use Ireps;
with Symbol_Table_Info;     use Symbol_Table_Info;

with Tree_Walk;             use Tree_Walk;
with Gather_Irep_Symbols;

with GNATCOLL.JSON;         use GNATCOLL.JSON;

with Sinfo;                 use Sinfo;
with Namet;                 use Namet;
with Lib;                   use Lib;

with GNAT_Utils;            use GNAT_Utils;
with GNAT2GOTO.Options;


--  NB warnings:
--  Integers must be passed to e.g. Make_Int_Constant_Expr as hexadecial, e.g. 6F (no need for Ox)
--  NB 'declarations' seem to be optional - so in some cases I use them, in others I don't

package body Minimal_01 is

   procedure SayHello is
   begin
      Put_Line ("procedure SayHello is saying: hello! 2019-01-25_14-58-17");
   end SayHello;

   function Make_Int32_Type return Irep is
   begin
      --  The below two initialisations are equivalent (first is a signedbv wrapper)
      --  return Make_Int_Type (32);
      return Make_Signedbv_Type (Ireps.Empty, 32);
   end;

   function Make_Int_Constant_expr(Int_Value : String) return Irep is
   begin
      return Make_Constant_Expr (Source_Location => 0,
                                 I_Type          => Make_Int32_Type,
                                 Range_Check     => False,
                                 Value           =>Int_Value);
   end;
   
   -- (Do not call 'make_var' directly - unless you pass it a type)
   -- Create a "variable" - an irep and associated symbol (joined by identifer)
   -- and place symbol in symbol table
   function Make_Var(Var_Type_p : Irep; Name : String) return Irep is
      Symbol_Irep : Irep := New_Irep (I_Symbol_Expr);
      Var_Name : constant String := Name;
      Var : Symbol;
      Var_Type : constant Irep := Var_Type_p;
   begin
      -- these are ESSENTIAL!
      -- symbol and irep(I_Symbol_Expr) with identifier==symbol.Name are both required
      Set_Identifier (Symbol_Irep, Var_Name);
      Set_Type (Symbol_Irep, Var_Type);

      Var.SymType := Var_Type;
      --  Var.Value := Var_Irep; -- not essential for a decalred variable, required for functions
      Var.Name          := Intern (Var_Name);
      Var.PrettyName    := Var.Name;
      Var.BaseName      := Var.Name;
      Var.SymType       := Var_Type;
      Var.IsThreadLocal := True;
      Var.IsFileLocal   := True;
      Var.IsLValue      := True;
      Var.IsParameter   := False;

      Global_Symbol_Table.Insert (Var.Name, Var);
      return Symbol_Irep;
   end;

   function Make_Var_Int(Name: String) return Irep is
      Var_Type : constant Irep := Make_Int32_Type;
   begin
      return Make_Var(Var_Type, Name);
   end;
   
   function Make_Var_Int_Ptr(name : String) return Irep is
      Var_Type : constant Irep :=
        Make_Pointer_Type(Base => Make_Int32_Type);
   begin
      return Make_Var(Var_Type, Name);
   end;
   
   function Make_Var_Bool(name : String) return Irep is
      Var_Type : constant Irep := Make_Bool_Type;
   begin
      return Make_Var(Var_Type, Name);      
   end;
      
   --  ======================================================================
   procedure Test01 is
      --  ADD STUFF TO PROCEDURE "DUMMY"
      --  ============================================================
      Map_Index : constant Symbol_Id := Intern("dummy");
      Dummy_Symbol : Symbol := Global_Symbol_Table (Map_Index);
      Dummy_Body : Irep;
      --  ============================================================
      
      Var1_Symbol_Irep : Irep := Make_Var_Int("var1_user");
      Var2_Symbol_Irep : Irep := Make_Var_Int("var2_user");
      
      Var3_Symbol_Irep : Irep := Make_Var_Int("var3_user");
      
      --  Below is the beginnings of an attempt to define the type for a literal array
      --  (not enought time to finish atm)
      --  Bare_Array_Type : constant Irep :=
      --    Make_Array_Type (I_Subtype => Element_Type,
      --                     Size => Len_Expr);
      --  Var3_Symbol_Irep : Irep := Make_Array_Expr (I_Type => Bare_Array_Type, 
      --                                              Source_Location => 0);
      
      Var4_Symbol_Irep : Irep := Make_Var_Int_Ptr("var4_user");
      
      Decl1 : constant Irep := New_Irep (I_Code_Decl);
      Decl2 : constant Irep := New_Irep (I_Code_Decl);
      Decl3 : constant Irep := New_Irep (I_Code_Decl);
      
      Op_Equal_Var1 : Irep;
      Op_Equal_Var2 : Irep;
      Op_Equal_Var3 : Irep;
      
      Assign_Result1 : Irep;
      Assign_Result2 : Irep;
      Assign_Result3 : Irep;
      Assign_Result4 : Irep;

      A_Irep1 : Irep;
      A_Irep2 : Irep;
      
      --  variable declarations for let expr
      X_Symbol_Irep : Irep;
      Integer_Three : Irep;
      Irep_add : Irep;
      Let_Expr_Irep : Irep;
      let_expr_result : Irep;
      Op_Assign_Let_Expr : Irep;
      Op_Equal_Let_Expr : Irep;
      A_Irep_Let_Expr : Irep;
      
      ----------------------------------------
      Global_Irep_Array_Op_Zero_Index_Local : Irep :=  Global_Irep_Array_Op_Zero_Index;
      Global_Irep_Array_Op_Deref_Local : Irep :=  Global_Irep_Array_Op_Deref;
      Global_Irep_Do_Aggregate_Literal_Array_Array_expr_Local : Irep :=
        Global_Irep_Do_Aggregate_Literal_Array_Array_Expr;

      Global_Irep_Dup_Array_Ptr_Param_Local : Irep :=
        Global_Irep_Dup_Array_Ptr_Param;
      ----------------------------------------
      int_ptr_var : Irep := Make_Var_Int_ptr("var_user_int_ptr");
      ----------------------------------------
   begin
      Put_Line ("procedure test02");

      if Global_Symbol_Table.Contains (Map_Index) then
         Put_line("Found dummy procedure!");
         Dummy_Body := Dummy_Symbol.Value;
         
         ----------------------------------------
         --  ASSERT ON ARRAY INDEX (MUST BE ZERO)
         
         Set_Source_Location (Decl1, 0);
         Set_Symbol (Decl1, Var1_Symbol_Irep);
         Append_Op (Dummy_Body, Decl1);
         
         Assign_Result1 :=
           Make_Code_Assign (Rhs             => Global_Irep_Array_Op_Zero_Index_Local,
                             Lhs             => Var1_Symbol_Irep,
                             Source_Location => 0,
                             I_Type          => Make_Int32_Type,
                             Range_Check     => False);

         Append_Op (Dummy_Body, Assign_Result1);

         Op_Equal_Var1 :=
           Make_Op_Eq (Rhs             => Make_Int_Constant_Expr("0"),
                       Lhs             => Var1_Symbol_Irep,
                       Source_Location => 0,
                       Overflow_Check  => False,
                       I_Type          => Make_Bool_Type,
                       Range_Check     => False);

         A_Irep1 := New_Irep (I_Code_Assert);
         Set_Assertion(A_Irep1, Op_Equal_Var1);
         Append_Op (Dummy_Body, A_Irep1);
         
         ----------------------------------------
         --  ASSERT ON DEREFERENCED VALUE (WILL FAIL verification!)
         
         Set_Source_Location (Decl2, 0);
         Set_Symbol (Decl2, Var2_Symbol_Irep);
         Append_Op (Dummy_Body, Decl2);

         Assign_Result2 := 
           Make_Code_Assign (Rhs             => Global_Irep_Array_Op_Deref_Local,
                             Lhs             => Var2_Symbol_Irep,
                             Source_Location => 0,
                             I_Type          => Make_Int32_Type,
                             Range_Check     => False);
         
         Append_Op (Dummy_Body, Assign_Result2);

         Op_Equal_Var2 :=
           Make_Op_Eq (Rhs             => Make_Int_Constant_Expr("B"),
                       Lhs             => Var2_Symbol_Irep,
                       Source_Location => 0,
                       Overflow_Check  => False,
                       I_Type          => Make_Bool_Type,
                       Range_Check     => False);

         A_Irep2 := New_Irep (I_Code_Assert);
         Set_Assertion(A_Irep2, Op_Equal_var2);
         Append_Op (Dummy_Body, A_Irep2);
         
         ----------------------------------------
         --  SHOW ARRAY LITERAL - commented out see comments below
         --  TODO lhs needs to be 'bare_array_type to pass the symex 
         --  (both side of assignment must be equal), the 
         --  below is however enought to display the array literal in the symbol table
         --  (despite symex errors)

         --  Assign_Result3 :=
         --    Make_Code_Assign (Rhs             =>
         --                      Global_Irep_Do_Aggregate_Literal_Array_Array_expr_Local,
         --                      Lhs             => Var3_Symbol_Irep,
         --                      Source_Location => 0,
         --                      I_Type          => Make_Int32_Type,
         --                      Range_Check     => False);

         --  Append_Op (Dummy_Body, Assign_result3);

         ----------------------------------------
         --  --  POINTER PARAM TO ARRAY DUP (commented out to hide noise atm)
         --  --  uncomment to get output...
         --  
         --  --  (nb this is likely being accessed in the wrong way/place so the output
         --  of below is not (yet) conclusive evidence of what/where the let expression
         --  binding of the array literal is going wrong.

         Assign_Result4 :=
           Make_Code_Assign (Rhs             =>
                             Global_Irep_Dup_Array_Ptr_Param_local,
                             Lhs             => Var4_symbol_Irep,
                             Source_Location => 0,
                             I_Type          =>
                             Make_Pointer_Type(Base => Make_Int32_Type),
                             Range_Check     => False);
         Append_Op (Dummy_Body, Assign_result4);
         
         ----------------------------------------
         -- CREATE A "LET EXPRESSION"
         --let x=44 in y = x + 3;
         
         X_Symbol_Irep := Make_Var_Int("x");
         Integer_Three := Make_Int_Constant_Expr("3");
         
         Irep_add := Make_Op_Add (Rhs             => Integer_three,
                                  Lhs             => X_Symbol_Irep,
                                  Source_Location => 0,
                                  Overflow_Check  => False,
                                  I_Type          => Make_Int32_Type,
                                  Range_Check     => False); 
         
         --  nb "2C" is 44 in hexadecimal
         Let_Expr_Irep := Make_Let_Expr
           (Symbol          => X_symbol_irep,
            Value           => Make_Int_Constant_Expr("2C"),
            Where           => Irep_add,
            Source_Location => 0,
            I_Type          => Make_Int32_Type,
            Range_Check     => False);

         Let_Expr_Result := Make_Var_Int("let_expr_result_var");
         
         Op_Assign_Let_Expr :=
           Make_Code_Assign (Rhs             => Let_Expr_Irep,
                             Lhs             => let_expr_result,
                             Source_Location => 0,
                             I_Type          => Make_Int32_Type,
                             Range_Check     => False);

         Append_Op (Dummy_Body, Op_Assign_Let_Expr);
         
         --  ASSERT ON THE LET EXPR OUTPUT (TO VERIFY)
         
         Op_Equal_Let_expr :=
           Make_Op_Eq (Rhs             => Make_Int_Constant_Expr("2F"),
                       Lhs             => Let_Expr_Result,
                       --  Lhs             => Total,
                       Source_Location => 0,
                       Overflow_Check  => False,
                       I_Type          => Make_Bool_Type,
                       Range_Check     => False);
         
         A_Irep_Let_Expr := New_Irep (I_Code_Assert);
         Set_Assertion(A_Irep_Let_Expr, Op_Equal_Let_expr);
         Append_Op (Dummy_Body, A_Irep_Let_Expr);
         
         ----------------------------------------
         
         Dummy_Symbol.Value := Dummy_Body;
      else
         Put_line("dummy not found, hmm...");
      end if;
   end;
   --  ----------------------------------------------------------------------
end Minimal_01;
