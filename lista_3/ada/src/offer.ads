with Ada.Containers.Vectors;
with Ada.Containers.Indefinite_Vectors;

package Offer is
   type Offer_Data (L : Natural; J : Natural; Cost_J : Natural) is record
      null;
   end record;

   type Offer is access Offer_Data;

   package Offer_Vector is new Ada.Containers.Vectors
     (Index_Type => Natural, Element_Type => Offer);
end Offer;
