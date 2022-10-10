with Ada.Containers.Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Aoc_Utils is
   package Input_Type is new Ada.Containers.Vectors (
      Index_Type => Natural,
      Element_Type => Unbounded_String);

   Assertion_Error : exception;
   procedure Assert (Expr : Boolean; Msg : String);
end Aoc_Utils;
