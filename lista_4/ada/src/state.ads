with Ada.Containers.Vectors;
with Ada.Containers.Indefinite_Vectors;

package State is
   type State_Data (Changed : Boolean; Nexthop : Natural; Cost : Natural)
   is record
      null;
   end record;

   type State is access State_Data;

   package State_Vector is new Ada.Containers.Vectors
     (Index_Type => Natural, Element_Type => State);
end State;
