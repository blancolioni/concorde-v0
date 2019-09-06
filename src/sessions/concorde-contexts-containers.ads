private generic
   Container_Name : String;
   type Context_Reference is private;
   with procedure Iterate_Children
     (Context : Context_Reference;
      Process : not null access
        procedure (Child : Context_Type));
package Concorde.Contexts.Containers is

   function Container_Context
     (Reference : Context_Reference)
     return Context_Type;

end Concorde.Contexts.Containers;
