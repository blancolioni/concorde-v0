private with Ada.Containers.Indefinite_Doubly_Linked_Lists;

package Nazar.Models is

   type Model_Observer_Interface is interface;

   procedure Notify
     (Observer : Model_Observer_Interface)
   is abstract;

   type Root_Model_Type is abstract tagged private;

   subtype Model_Class is Root_Model_Type'Class;

   type Model_Type is access all Root_Model_Type'Class;

   procedure Add_Observer
     (Model : in out Root_Model_Type'Class;
      Observer : Model_Observer_Interface'Class);

   procedure Remove_Observer
     (Model    : in out Root_Model_Type'Class;
      Observer : Model_Observer_Interface'Class);

   procedure Notify_Observers
     (Model : Root_Model_Type'Class);

private

   package Observer_Lists is
     new Ada.Containers.Indefinite_Doubly_Linked_Lists
       (Model_Observer_Interface'Class);

   type Root_Model_Type is abstract tagged
      record
         Observers : Observer_Lists.List;
      end record;

end Nazar.Models;
