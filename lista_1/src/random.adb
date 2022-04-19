with Ada.Numerics.Discrete_Random;
with Ada.Numerics.Float_Random;
with Constants;

package body Random is
   function Rand_Range (MIN : Natural; MAX : Natural) return Natural is
      subtype Boundries is Natural range MIN .. MAX;
      package Rand_Num is new Ada.Numerics.Discrete_Random (Boundries);
      Gen : Rand_Num.Generator;
   begin
      Rand_Num.Reset (Gen);
      return Rand_Num.Random (Gen);
   end Rand_Range;

   function Rand_Time return Duration is
      Gen : Ada.Numerics.Float_Random.Generator;
   begin
      Ada.Numerics.Float_Random.Reset (Gen);
      return
        Duration (Constants.Min_Time) +
        Duration
          ((Constants.Max_Time - Constants.Min_Time) *
           Ada.Numerics.Float_Random.Random (Gen));
   end Rand_Time;
end Random;
