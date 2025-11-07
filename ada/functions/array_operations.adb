-- Array operations in Ada

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Array_Operations is
   type Int_Array is array (1 .. 5) of Integer;

   Numbers : Int_Array := (1, 2, 3, 4, 5);
   Sum : Integer := 0;
   Product : Integer := 1;
   Max_Val : Integer;
   Min_Val : Integer;

begin
   Put_Line("");
   Put_Line("=== Array Operations in Ada ===");
   Put_Line("");

   -- Print array
   Put("Numbers: ");
   for I in Numbers'Range loop
      Put(Numbers(I), Width => 0);
      Put(" ");
   end loop;
   New_Line;

   -- Array length
   Put("Length: ");
   Put(Numbers'Length, Width => 0);
   New_Line;

   -- Access elements
   Put("First: ");
   Put(Numbers(Numbers'First), Width => 0);
   New_Line;

   Put("Last: ");
   Put(Numbers(Numbers'Last), Width => 0);
   New_Line;

   -- Sum
   for I in Numbers'Range loop
      Sum := Sum + Numbers(I);
   end loop;

   New_Line;
   Put("Sum: ");
   Put(Sum, Width => 0);
   New_Line;

   -- Product
   for I in Numbers'Range loop
      Product := Product * Numbers(I);
   end loop;

   Put("Product: ");
   Put(Product, Width => 0);
   New_Line;

   -- Max
   Max_Val := Numbers(Numbers'First);
   for I in Numbers'Range loop
      if Numbers(I) > Max_Val then
         Max_Val := Numbers(I);
      end if;
   end loop;

   Put("Maximum: ");
   Put(Max_Val, Width => 0);
   New_Line;

   -- Min
   Min_Val := Numbers(Numbers'First);
   for I in Numbers'Range loop
      if Numbers(I) < Min_Val then
         Min_Val := Numbers(I);
      end if;
   end loop;

   Put("Minimum: ");
   Put(Min_Val, Width => 0);
   New_Line;

   -- Squared
   New_Line;
   Put("Squared: ");
   for I in Numbers'Range loop
      Put(Numbers(I) * Numbers(I), Width => 0);
      Put(" ");
   end loop;
   New_Line;

end Array_Operations;
