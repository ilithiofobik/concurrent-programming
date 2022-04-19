with Ada.Containers.Vectors;
with Offer;

package Graph is
   package Natural_Vector is new Ada.Containers.Vectors
     (Index_Type => Natural, Element_Type => Natural);

   type Packet_Data (ID : Natural; H : Natural) is record
      Visited_Nodes : Natural_Vector.Vector;
      HP            : Natural := H;
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

   task type Proper_Receiver (I : Natural; My_Node : Node) is
      entry Receiver_Lock (New_Offer : Offer.Offer);
      entry Receive_Unlock;
   end Proper_Receiver;
   type Receiver is access Proper_Receiver;

   task type Proper_Manager (I : Natural; My_Node : Node) is
      entry Sender_Lock;
      entry Receive_Lock (New_Offer : Offer.Offer);
   end Proper_Manager;
   type Manager is access Proper_Manager;

   task type Proper_Sender (I : Natural; My_Node : Node) is
      entry Sender_Unlock (List : Offer.Offer_Vector.Vector);
   end Proper_Sender;
   type Sender is access Proper_Sender;

   package Receiver_Vector is new Ada.Containers.Vectors
     (Index_Type => Natural, Element_Type => Receiver);
   use Receiver_Vector;

   package Manager_Vector is new Ada.Containers.Vectors
     (Index_Type => Natural, Element_Type => Manager);
   use Manager_Vector;

   package Sender_Vector is new Ada.Containers.Vectors
     (Index_Type => Natural, Element_Type => Sender);
   use Sender_Vector;

   type Node_Data (ID : Natural) is record
      Packets_Passed : Natural_Vector.Vector;
      Successors     : Natural_Vector.Vector;
      My_Sender      : Sender;
      My_Receiver    : Receiver;
      My_Manager     : Manager;
   end record;

   type Graph is record
      Nodes : Node_Vector.Vector;
   end record;

   function Graph_Generator (N : Positive; D : Natural) return Graph;

   procedure Print_Graph (G : in Graph);

   Receivers : Receiver_Vector.Vector;
   Managers  : Manager_Vector.Vector;
   Senders   : Sender_Vector.Vector;
   Global_N  : Natural := 0;

   task type Proper_Counter is
      entry Increment;
      entry Decrement;
   end Proper_Counter;
   type Counter is access Proper_Counter;

   Global_Counter : Counter;
   Global_Finish  : Boolean := False;
   pragma Atomic (Global_Finish);
end Graph;
