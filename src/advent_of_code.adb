with Ada.Text_IO;
with Ada.Command_Line;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Aoc_Utils; use Aoc_Utils;
with Day_1;
with Day_2;

procedure Advent_Of_Code is
   package IO renames Ada.Text_IO;
   package CLI renames Ada.Command_Line;

   function Get_Input (File_Name : String) return Input_Type.Vector is
      F : IO.File_Type;
      Result : Input_Type.Vector := Input_Type.Empty_Vector;
   begin
      IO.Open (F, IO.In_File, File_Name);
      while not IO.End_Of_File (F) loop
         Input_Type.Append (Result, To_Unbounded_String (IO.Get_Line (F)));
      end loop;
      return Result;
   end Get_Input;

   Day : Natural;
begin
   if CLI.Argument_Count /= 2 then
      IO.Put_Line ("advent of code 2021, by Nicole Mazzuca");
      IO.Put_Line ("USAGE");
      IO.Put_Line ("      " & CLI.Command_Name & " <day> <input-file>");
      CLI.Set_Exit_Status (CLI.Failure);
   else
      Day := Natural'Value (CLI.Argument (1));

      declare
         Input : constant Input_Type.Vector := Get_Input (CLI.Argument (2));
      begin
         case Day is
            when 1 => Day_1 (Input);
            when 2 => Day_2 (Input);
            when others => IO.Put_Line (
               "Unimplemented day: " & Natural'Image (Day));
         end case;
      end;
   end if;
end Advent_Of_Code;
