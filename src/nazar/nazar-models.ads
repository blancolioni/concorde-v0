private with Ada.Containers.Indefinite_Doubly_Linked_Lists;

with Nazar.Interfaces.Properties;

package Nazar.Models is

   type Model_Observer_Interface is interface;

   procedure Notify
     (Observer : Model_Observer_Interface)
   is abstract;

   type Root_Model_Type is
     abstract new Nazar.Interfaces.Properties.Property_Container_Interface
     and Nazar_Object_Interface
   with private;

   subtype Model_Class is Root_Model_Type'Class;

   type Model_Type is access all Root_Model_Type'Class;

   procedure Initialize
     (Model : in out Root_Model_Type);

   procedure Add_Observer
     (Model : in out Root_Model_Type'Class;
      Observer : Model_Observer_Interface'Class);

   procedure Remove_Observer
     (Model    : in out Root_Model_Type'Class;
      Observer : Model_Observer_Interface'Class);

   procedure Notify_Observers
     (Model : Root_Model_Type'Class);

   function Null_Model return Root_Model_Type'Class;

private

   package Observer_Lists is
     new Ada.Containers.Indefinite_Doubly_Linked_Lists
       (Model_Observer_Interface'Class);

   type Root_Model_Type is
     abstract new Nazar.Interfaces.Properties.Root_Property_Container
       and Nazar_Object_Interface with
      record
         Id         : WL.Guids.Guid;
         Observers  : Observer_Lists.List;
      end record;

   overriding function Guid
     (Model : Root_Model_Type)
      return WL.Guids.Guid
   is (Model.Id);

end Nazar.Models;
