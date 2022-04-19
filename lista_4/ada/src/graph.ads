with Ada.Containers.Vectors;
with Ada.Containers.Indefinite_Vectors;
with Ada.Containers.Synchronized_Queue_Interfaces;
with Ada.Containers.Unbounded_Synchronized_Queues;
with Ada.Strings.Unbounded;
with Offer;

package Graph is
   package Natural_Vector is new Ada.Containers.Vectors
     (Index_Type => Natural, Element_Type => Natural);

   type Standard_Packet_Data
     (RS : Natural; HS : Natural; RD : Natural; HD : Natural) is record
      Visited_Routers : Ada.Strings.Unbounded.Unbounded_String :=
        Ada.Strings.Unbounded.To_Unbounded_String ("");
   end record;

   type Standard_Packet is access Standard_Packet_Data;

   package Standard_Packet_Queue_Interfaces is new Ada.Containers
     .Synchronized_Queue_Interfaces
     (Element_Type => Standard_Packet);

   package Standard_Packet_Queues is new Ada.Containers
     .Unbounded_Synchronized_Queues
     (Queue_Interfaces => Standard_Packet_Queue_Interfaces);

   type Standard_Packet_Queues_Access is access Standard_Packet_Queues.Queue;

   package Standard_Packet_Queue_Vector is new Ada.Containers.Vectors
     (Index_Type => Natural, Element_Type => Standard_Packet_Queues_Access);

   Queues : Standard_Packet_Queue_Vector.Vector;

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
      entry Forwarder_Lock (Idx : in Natural; Next_Hop : out Natural);
   end Proper_Manager;
   type Manager is access Proper_Manager;

   task type Proper_Forwarder_Sender (R : Natural; My_Node : Node);
   type Forwarder_Sender is access Proper_Forwarder_Sender;

   task type Proper_Forwarder_Receiver (R : Natural; My_Node : Node) is
      entry Forwarder_Get (Packet : Standard_Packet);
   end Proper_Forwarder_Receiver;
   type Forwarder_Receiver is access Proper_Forwarder_Receiver;

   task type Proper_Host (R : Natural; H : Natural; My_Node : Node) is
      entry Run (Init_R : Natural; Init_H : Natural);
      entry Host_Get (Packet : Standard_Packet);
   end Proper_Host;
   type Host is access Proper_Host;

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

   package Forwarder_Sender_Vector is new Ada.Containers.Vectors
     (Index_Type => Natural, Element_Type => Forwarder_Sender);
   use Forwarder_Sender_Vector;

   package Forwarder_Receiver_Vector is new Ada.Containers.Vectors
     (Index_Type => Natural, Element_Type => Forwarder_Receiver);
   use Forwarder_Receiver_Vector;

   package Host_Vector is new Ada.Containers.Vectors
     (Index_Type => Natural, Element_Type => Host);
   use Host_Vector;

   type Node_Data (ID : Natural) is record
      Packets_Passed        : Natural_Vector.Vector;
      Successors            : Natural_Vector.Vector;
      My_Sender             : Sender;
      My_Receiver           : Receiver;
      My_Manager            : Manager;
      My_Forwarder_Sender   : Forwarder_Sender;
      My_Forwarder_Receiver : Forwarder_Receiver;
      My_Forwarder_Queue    : Standard_Packet_Queues.Queue;
      My_Hosts              : Host_Vector.Vector;
      Number_Of_Hosts       : Natural := 0;
   end record;

   type Graph is record
      Nodes : Node_Vector.Vector;
   end record;

   function Graph_Generator
     (N : Positive; D : Natural; H : Natural) return Graph;

   procedure Print_Graph (G : in Graph);

   Receivers           : Receiver_Vector.Vector;
   Managers            : Manager_Vector.Vector;
   Senders             : Sender_Vector.Vector;
   Forwarder_Receivers : Forwarder_Receiver_Vector.Vector;
   Global_N            : Natural := 0;

end Graph;
