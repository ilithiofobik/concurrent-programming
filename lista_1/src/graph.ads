with Ada.Containers.Vectors;

package Graph is
   package Natural_Vector is new Ada.Containers.Vectors
     (Index_Type => Natural, Element_Type => Natural);

   type Packet_Data (ID : Natural; TTL : Natural) is record
      Visited_Nodes : Natural_Vector.Vector;
   end record;

   type Packet is access Packet_Data;

   package Packet_Vector is new Ada.Containers.Vectors
     (Index_Type => Natural, Element_Type => Packet);
   use Packet_Vector;

   type Node_Data;
   type Node is access Node_Data;

   package Node_Vector is new Ada.Containers.Vectors
     (Index_Type => Natural, Element_Type => Node);
   use Node_Vector;

   task type Proper_Printer (K : Natural) is
      entry Print (Text : String; Is_Received : Boolean);
      entry Raports (Nodes : Node_Vector.Vector; Packs : Packet_Vector.Vector);
   end Proper_Printer;
   type Printer is access Proper_Printer;

   task type Proper_Hunter (K : Natural);

   type Hunter is access Proper_Hunter;

   task type Proper_Vertice (My_Node : Node; My_Printer : Printer) is
      entry Receive (Pack : Packet);
      entry SetTrap (Set : Boolean);
   end Proper_Vertice;

   type Vertice is access Proper_Vertice;

   type Node_Data (ID : Natural) is record
      Packets_Passed : Natural_Vector.Vector;
      Successors     : Node_Vector.Vector;
      My_Vertice     : Vertice;
   end record;

   type Graph is record
      Nodes : Node_Vector.Vector;
   end record;

   function Graph_Generator (N : Positive; D : Natural) return Graph;

   procedure Print_Graph (G : in Graph);

end Graph;
