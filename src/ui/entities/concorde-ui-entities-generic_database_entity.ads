with Concorde.Db.Has_Name;

generic
   type Container_Handle is private;
   type Record_Reference is private;
   Null_Record_Reference : Record_Reference;
   type Record_Interface is
     abstract limited new Concorde.Db.Has_Name.Has_Name_Interface with private;
   with function Get_Record
     (Reference : Record_Reference)
     return Record_Interface'Class;
   with function Get_Reference
     (Reference : Record_Interface'Class)
      return Record_Reference;
   with function Get_Reference_By_Name
     (Handle : Container_Handle;
      Name   : String)
      return Record_Reference;
   with procedure Iterate
     (Handle  : Container_Handle;
      Process : not null access
        procedure (Rec : Record_Interface'Class));
   with function Contents
     (Item : Record_Interface'Class)
     return String;
   with function "=" (Left, Right : Record_Reference) return Boolean is <>;
package Concorde.UI.Entities.Generic_Database_Entity is

   function Get_Record_Node
     (Reference : Record_Reference)
      return Entity_Reference;

   function Get_Container_Node
     (Handle : Container_Handle)
      return Entity_Reference;

end Concorde.UI.Entities.Generic_Database_Entity;
