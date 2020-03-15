with Nazar.Views.Gtk_Views;

package Nazar.Gtk_Main is

   procedure Schedule_Update
     (View : not null access
        Nazar.Views.Gtk_Views.Nazar_Gtk_View_Record'Class);

   procedure Execute_Updates;

end Nazar.Gtk_Main;
