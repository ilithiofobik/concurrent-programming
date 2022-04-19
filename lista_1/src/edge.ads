with Ada.Containers.Vectors;
with Ada.Containers.Indefinite_Vectors;

package Edge is
   type Int_Edge_Data (From : Natural; To : Natural) is record
      null;
   end record;

   type Int_Edge is access Int_Edge_Data;

   package Edge_Vector is new Ada.Containers.Vectors
     (Index_Type => Natural, Element_Type => Int_Edge);
end Edge;
