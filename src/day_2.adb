with Ada.Text_IO;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Vectors;
with Ada.Strings;
with Ada.Strings.Maps;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Aoc_Utils; use Aoc_Utils;

procedure Day_2 (Input : Input_Type.Vector) is
   package IO renames Ada.Text_IO;

   type Direction_Type is (Up, Down, Forward);
   type Command is record
      Direction : Direction_Type;
      Count : Natural;
   end record;

   package Data_Type is new Vectors (
      Index_Type => Positive,
      Element_Type => Command);

   procedure Day_2a (Data : Data_Type.Vector) is
      Depth : Integer := 0;
      Position : Natural := 0;
   begin
      for Element of Data loop
         case Element.Direction is
            when Down => Depth := Depth + Element.Count;
            when Up => Depth := Depth - Element.Count;
            when Forward => Position := Position + Element.Count;
         end case;
      end loop;
      IO.Put_Line (
         "Day 2a: depth = "
         & Depth'Image
         & ", pos = "
         & Position'Image
         & ", product = "
         & Natural'Image (Position * Depth));
   end Day_2a;

   procedure Day_2b (Data : Data_Type.Vector) is
      Aim : Integer := 0;
      Depth : Integer := 0;
      Position : Natural := 0;
   begin
      for Element of Data loop
         case Element.Direction is
            when Down => Aim := Aim + Element.Count;
            when Up => Aim := Aim - Element.Count;
            when Forward =>
               Position := Position + Element.Count;
               Depth := Depth + (Aim * Element.Count);
         end case;
      end loop;
      IO.Put_Line (
         "Day 2b: depth = "
         & Depth'Image
         & ", pos = "
         & Position'Image
         & ", product = "
         & Natural'Image (Position * Depth));
   end Day_2b;

   function Parse_Line (Line : String) return Command is
      Result : Command;
      Space_Index : constant Natural := Index (Line, Ada.Strings.Maps.To_Set (Ada.Strings.Space));
      Direction_Str : constant String := Line (Line'First .. Space_Index - 1);
      Count_Str : constant String := Line (Space_Index + 1 .. Line'Last);
   begin
      if Direction_Str = "up" then
         Result.Direction := Up;
      elsif Direction_Str = "down" then
         Result.Direction := Down;
      elsif Direction_Str = "forward" then
         Result.Direction := Forward;
      else
         Assert (False, "Invalid direction: " & Direction_Str);
      end if;

      Result.Count := Natural'Value (Count_Str);
      return Result;
   end Parse_Line;

   Data : Data_Type.Vector := Data_Type.Empty_Vector;
begin
   for S of Input loop
      Data.Append (Parse_Line (To_String (S)));
   end loop;

   Day_2a (Data);
   Day_2b (Data);
end Day_2;
