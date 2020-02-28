with Ada.Strings.Unbounded;

package Nazar.Views.Labels is

   type Label_View_Type is new Root_View_Type with private;

   subtype Label_View_Class is Label_View_Type'Class;

   type Label_View is access all Label_View_Type'Class;

private

   type Label_View_Type is
     new Root_View_Type with
      record
         Text : Ada.Strings.Unbounded.Unbounded_String;
      end record;

end Nazar.Views.Labels;
