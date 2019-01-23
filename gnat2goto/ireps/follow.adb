package body Follow is
   function Follow_Symbol_Type (I : Irep; ST : Symbol_Table) return Irep is
      Next : Irep := I;
   begin
      while Next /= 0 and then Kind (Next) = I_Symbol_Type loop
         Next := ST (Intern (Get_Identifier (Next))).SymType;
      end loop;
      if Kind (Next) = I_Bounded_Signedbv_Type then
         Next :=
           Make_Signedbv_Type (I_Subtype => Get_Subtype (Next),
                               Width     => Get_Width (Next));
      end if;
      return Next;
   end Follow_Symbol_Type;
end Follow;
