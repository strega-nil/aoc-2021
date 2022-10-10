package body Aoc_Utils is
   procedure Assert (Expr : Boolean; Msg : String) is
   begin
      if not Expr then
         raise Assertion_Error with Msg;
      end if;
   end Assert;
end Aoc_Utils;
