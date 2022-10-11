with Ada.Text_IO;
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Vectors;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Maps;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Aoc_Utils; use Aoc_Utils;

procedure Day_4 (Input : Input_Type.Vector) is
   package IO renames Ada.Text_IO;

   type Bingo_Space is record
      Value : Natural;
      Marked : Boolean := False;
   end record;

   type Bingo_Column is array (1 .. 5) of Bingo_Space;
   type Bingo_Board is array (1 .. 5) of Bingo_Column;

   package Bingo_Boards is new Vectors (
      Index_Type => Positive,
      Element_Type => Bingo_Board);
   package Bingo_Values is new Vectors (
      Index_Type => Positive,
      Element_Type => Natural);

   type Data_Type is record
      Values : Bingo_Values.Vector;
      Boards : Bingo_Boards.Vector;
   end record;

   function Is_Complete (Board : Bingo_Board) return Boolean is
      All_True : Boolean;
   begin
      --  check columns
      for Column of Board loop
         All_True := True;
         for Box of Column loop
            if not Box.Marked then
               All_True := False;
            end if;
         end loop;
         if All_True then
            return True;
         end if;
      end loop;

      --  check rows
      for J in 1 .. 5 loop
         All_True := True;
         for I in 1 .. 5 loop
            if not Board (I)(J).Marked then
               All_True := False;
            end if;
         end loop;
         if All_True then
            return True;
         end if;
      end loop;

      return False;
   end Is_Complete;

   procedure Put_Line (Board : Bingo_Board) is
   begin
      for Column of Board loop
         for Box of Column loop
            if Box.Value < 10 then
               IO.Put (" " & Box.Value'Image);
            else
               IO.Put (Box.Value'Image);
            end if;
            IO.Put (if Box.Marked then "|" else " ");
         end loop;
         IO.Put_Line ("");
      end loop;
   end Put_Line;

   procedure Mark (Board : in out Bingo_Board; Value : Natural) is
   begin
      for Column of Board loop
         for Box of Column loop
            if Box.Value = Value then
               Box.Marked := True;
            end if;
         end loop;
      end loop;
   end Mark;

   function Score (Board : Bingo_Board) return Natural is
      Result : Natural := 0;
   begin
      for Column of Board loop
         for Box of Column loop
            if not Box.Marked then
               Result := Result + Box.Value;
            end if;
         end loop;
      end loop;

      return Result;
   end Score;

   procedure Day_4a (Data : Data_Type) is
      My_Data : Data_Type := Data;

      function Get_Winning_Board (Last : out Natural) return Bingo_Board is
      begin
         for Value of My_Data.Values loop
            for Board of My_Data.Boards loop
               Mark (Board, Value);
               if Is_Complete (Board) then
                  Last := Value;
                  return Board;
               end if;
            end loop;
         end loop;

         raise Assertion_Error with "no bingo boards are winning";
      end Get_Winning_Board;

      Last : Natural;
      Winning_Board : constant Bingo_Board := Get_Winning_Board (Last);
   begin
      IO.Put_Line ("Day 4a: winning board:");
      Put_Line (Winning_Board);
      IO.Put_Line ("score: "
         & Natural'Image (Score (Winning_Board))
         & ", last: "
         & Last'Image
         & ", prod: "
         & Natural'Image (Score (Winning_Board) * Last));
   end Day_4a;

   procedure Day_4b (Data : Data_Type) is
      function Get_Losing_Board (Last : out Natural) return Bingo_Board is
         Incomplete_Boards : Bingo_Boards.Vector := Data.Boards;
         Tmp : Bingo_Boards.Vector := Bingo_Boards.Empty_Vector;
      begin
         for Value of Data.Values loop
            for Board of Incomplete_Boards loop
               Mark (Board, Value);
               if not Is_Complete (Board) then
                  Tmp.Append (Board);
               end if;
            end loop;
            --  only one board left, and it won
            if Incomplete_Boards.Length = 1 and then Tmp.Length = 0 then
               Last := Value;
               return Incomplete_Boards (1);
            end if;
            Incomplete_Boards := Tmp;
            Tmp.Clear;
         end loop;

         raise Assertion_Error with "no bingo boards are losing";
      end Get_Losing_Board;

      Last : Natural;
      Losing_Board : constant Bingo_Board := Get_Losing_Board (Last);
   begin
      IO.Put_Line ("Day 4b: losing board:");
      Put_Line (Losing_Board);
      IO.Put_Line ("score: "
         & Natural'Image (Score (Losing_Board))
         & ", last: "
         & Last'Image
         & ", prod: "
         & Natural'Image (Score (Losing_Board) * Last));
   end Day_4b;

   function Parse_Values (Line : Unbounded_String)
      return Bingo_Values.Vector
   is
      Result : Bingo_Values.Vector := Bingo_Values.Empty_Vector;
      Comma_Set : constant Maps.Character_Set := Maps.To_Set (',');
      Current : Unbounded_String;
      Rest : Unbounded_String := Line;
      Comma_Index : Natural;
   begin
      loop
         Comma_Index := Index (Rest, Comma_Set);
         if Comma_Index /= 0 then
            Current := Unbounded_Slice (Rest, 1, Comma_Index - 1);
            Rest := Unbounded_Slice (Rest, Comma_Index + 1, Length (Rest));
            Result.Append (Natural'Value (To_String (Current)));
         else
            Result.Append (Natural'Value (To_String (Rest)));
            exit;
         end if;
      end loop;

      return Result;
   end Parse_Values;

   function Parse_Boards (
      Input : Input_Type.Vector;
      Start_Index : Positive
   ) return Bingo_Boards.Vector is
      Current_Board_Index : Positive := Start_Index;
      Current_Board : Bingo_Board;
      Current_Line : Unbounded_String;
      Result : Bingo_Boards.Vector := Bingo_Boards.Empty_Vector;
   begin
      loop
         exit when Current_Board_Index + 4 > Count_Type'Pos (Input.Length);
         for I in 1 .. 5 loop
            Current_Line := Input (Current_Board_Index + I - 1);
            for J in 1 .. 5 loop
               Current_Board (I)(J).Value :=
                  Natural'Value (Slice (Current_Line, J * 3 - 2, J * 3 - 1));
            end loop;
         end loop;
         Result.Append (Current_Board);
         Current_Board_Index := Current_Board_Index + 5;
      end loop;

      return Result;
   end Parse_Boards;

   Data : Data_Type;
begin
   Data.Values := Parse_Values (Input (1));
   Data.Boards := Parse_Boards (Input, 2);

   Day_4a (Data);
   Day_4b (Data);
end Day_4;
