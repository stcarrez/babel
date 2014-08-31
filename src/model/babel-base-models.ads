-----------------------------------------------------------------------
--  Babel.Base.Models -- Babel.Base.Models
-----------------------------------------------------------------------
--  File generated by ada-gen DO NOT MODIFY
--  Template used: templates/model/package-spec.xhtml
--  Ada Generator: https://ada-gen.googlecode.com/svn/trunk Revision 1095
-----------------------------------------------------------------------
--  Copyright (C) 2014 Stephane Carrez
--  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
--
--  Licensed under the Apache License, Version 2.0 (the "License");
--  you may not use this file except in compliance with the License.
--  You may obtain a copy of the License at
--
--      http://www.apache.org/licenses/LICENSE-2.0
--
--  Unless required by applicable law or agreed to in writing, software
--  distributed under the License is distributed on an "AS IS" BASIS,
--  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
--  See the License for the specific language governing permissions and
--  limitations under the License.
-----------------------------------------------------------------------
pragma Warnings (Off, "unit * is not referenced");
with ADO.Sessions;
with ADO.Objects;
with ADO.Statements;
with ADO.SQL;
with ADO.Schemas;
with Ada.Calendar;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;
with Util.Beans.Objects;
with Util.Beans.Basic.Lists;
pragma Warnings (On, "unit * is not referenced");
package Babel.Base.Models is
   type Store_Ref is new ADO.Objects.Object_Ref with null record;

   type Backup_Ref is new ADO.Objects.Object_Ref with null record;

   type FileSet_Ref is new ADO.Objects.Object_Ref with null record;

   type Path_Ref is new ADO.Objects.Object_Ref with null record;

   type File_Ref is new ADO.Objects.Object_Ref with null record;

   --  Create an object key for Store.
   function Store_Key (Id : in ADO.Identifier) return ADO.Objects.Object_Key;
   --  Create an object key for Store from a string.
   --  Raises Constraint_Error if the string cannot be converted into the object key.
   function Store_Key (Id : in String) return ADO.Objects.Object_Key;

   Null_Store : constant Store_Ref;
   function "=" (Left, Right : Store_Ref'Class) return Boolean;

   --
   procedure Set_Id (Object : in out Store_Ref;
                     Value  : in ADO.Identifier);

   --
   function Get_Id (Object : in Store_Ref)
                 return ADO.Identifier;

   --
   procedure Set_Name (Object : in out Store_Ref;
                       Value  : in Ada.Strings.Unbounded.Unbounded_String);
   procedure Set_Name (Object : in out Store_Ref;
                       Value : in String);

   --
   function Get_Name (Object : in Store_Ref)
                 return Ada.Strings.Unbounded.Unbounded_String;
   function Get_Name (Object : in Store_Ref)
                 return String;

   --
   procedure Set_Parameter (Object : in out Store_Ref;
                            Value  : in Ada.Strings.Unbounded.Unbounded_String);
   procedure Set_Parameter (Object : in out Store_Ref;
                            Value : in String);

   --
   function Get_Parameter (Object : in Store_Ref)
                 return Ada.Strings.Unbounded.Unbounded_String;
   function Get_Parameter (Object : in Store_Ref)
                 return String;

   --
   procedure Set_Server (Object : in out Store_Ref;
                         Value  : in Ada.Strings.Unbounded.Unbounded_String);
   procedure Set_Server (Object : in out Store_Ref;
                         Value : in String);

   --
   function Get_Server (Object : in Store_Ref)
                 return Ada.Strings.Unbounded.Unbounded_String;
   function Get_Server (Object : in Store_Ref)
                 return String;

   --  Load the entity identified by 'Id'.
   --  Raises the NOT_FOUND exception if it does not exist.
   procedure Load (Object  : in out Store_Ref;
                   Session : in out ADO.Sessions.Session'Class;
                   Id      : in ADO.Identifier);

   --  Load the entity identified by 'Id'.
   --  Returns True in <b>Found</b> if the object was found and False if it does not exist.
   procedure Load (Object  : in out Store_Ref;
                   Session : in out ADO.Sessions.Session'Class;
                   Id      : in ADO.Identifier;
                   Found   : out Boolean);

   --  Find and load the entity.
   overriding
   procedure Find (Object  : in out Store_Ref;
                   Session : in out ADO.Sessions.Session'Class;
                   Query   : in ADO.SQL.Query'Class;
                   Found   : out Boolean);

   --  Save the entity.  If the entity does not have an identifier, an identifier is allocated
   --  and it is inserted in the table.  Otherwise, only data fields which have been changed
   --  are updated.
   overriding
   procedure Save (Object  : in out Store_Ref;
                   Session : in out ADO.Sessions.Master_Session'Class);

   --  Delete the entity.
   overriding
   procedure Delete (Object  : in out Store_Ref;
                     Session : in out ADO.Sessions.Master_Session'Class);

   overriding
   function Get_Value (From : in Store_Ref;
                       Name : in String) return Util.Beans.Objects.Object;

   --  Table definition
   STORE_TABLE : constant ADO.Schemas.Class_Mapping_Access;

   --  Internal method to allocate the Object_Record instance
   overriding
   procedure Allocate (Object : in out Store_Ref);

   --  Copy of the object.
   procedure Copy (Object : in Store_Ref;
                   Into   : in out Store_Ref);

   --  Create an object key for Backup.
   function Backup_Key (Id : in ADO.Identifier) return ADO.Objects.Object_Key;
   --  Create an object key for Backup from a string.
   --  Raises Constraint_Error if the string cannot be converted into the object key.
   function Backup_Key (Id : in String) return ADO.Objects.Object_Key;

   Null_Backup : constant Backup_Ref;
   function "=" (Left, Right : Backup_Ref'Class) return Boolean;

   --
   procedure Set_Id (Object : in out Backup_Ref;
                     Value  : in ADO.Identifier);

   --
   function Get_Id (Object : in Backup_Ref)
                 return ADO.Identifier;

   --
   procedure Set_Create_Date (Object : in out Backup_Ref;
                              Value  : in Ada.Calendar.Time);

   --
   function Get_Create_Date (Object : in Backup_Ref)
                 return Ada.Calendar.Time;

   --
   procedure Set_Store (Object : in out Backup_Ref;
                        Value  : in Babel.Base.Models.Store_Ref'Class);

   --
   function Get_Store (Object : in Backup_Ref)
                 return Babel.Base.Models.Store_Ref'Class;

   --  Load the entity identified by 'Id'.
   --  Raises the NOT_FOUND exception if it does not exist.
   procedure Load (Object  : in out Backup_Ref;
                   Session : in out ADO.Sessions.Session'Class;
                   Id      : in ADO.Identifier);

   --  Load the entity identified by 'Id'.
   --  Returns True in <b>Found</b> if the object was found and False if it does not exist.
   procedure Load (Object  : in out Backup_Ref;
                   Session : in out ADO.Sessions.Session'Class;
                   Id      : in ADO.Identifier;
                   Found   : out Boolean);

   --  Find and load the entity.
   overriding
   procedure Find (Object  : in out Backup_Ref;
                   Session : in out ADO.Sessions.Session'Class;
                   Query   : in ADO.SQL.Query'Class;
                   Found   : out Boolean);

   --  Save the entity.  If the entity does not have an identifier, an identifier is allocated
   --  and it is inserted in the table.  Otherwise, only data fields which have been changed
   --  are updated.
   overriding
   procedure Save (Object  : in out Backup_Ref;
                   Session : in out ADO.Sessions.Master_Session'Class);

   --  Delete the entity.
   overriding
   procedure Delete (Object  : in out Backup_Ref;
                     Session : in out ADO.Sessions.Master_Session'Class);

   overriding
   function Get_Value (From : in Backup_Ref;
                       Name : in String) return Util.Beans.Objects.Object;

   --  Table definition
   BACKUP_TABLE : constant ADO.Schemas.Class_Mapping_Access;

   --  Internal method to allocate the Object_Record instance
   overriding
   procedure Allocate (Object : in out Backup_Ref);

   --  Copy of the object.
   procedure Copy (Object : in Backup_Ref;
                   Into   : in out Backup_Ref);

   --  Create an object key for FileSet.
   function FileSet_Key (Id : in ADO.Identifier) return ADO.Objects.Object_Key;
   --  Create an object key for FileSet from a string.
   --  Raises Constraint_Error if the string cannot be converted into the object key.
   function FileSet_Key (Id : in String) return ADO.Objects.Object_Key;

   Null_FileSet : constant FileSet_Ref;
   function "=" (Left, Right : FileSet_Ref'Class) return Boolean;

   --
   procedure Set_Id (Object : in out Fileset_Ref;
                     Value  : in ADO.Identifier);

   --
   function Get_Id (Object : in Fileset_Ref)
                 return ADO.Identifier;

   --
   procedure Set_First_Id (Object : in out Fileset_Ref;
                           Value  : in ADO.Identifier);

   --
   function Get_First_Id (Object : in Fileset_Ref)
                 return ADO.Identifier;

   --
   procedure Set_Last_Id (Object : in out Fileset_Ref;
                          Value  : in ADO.Identifier);

   --
   function Get_Last_Id (Object : in Fileset_Ref)
                 return ADO.Identifier;

   --
   procedure Set_Backup (Object : in out Fileset_Ref;
                         Value  : in Babel.Base.Models.Backup_Ref'Class);

   --
   function Get_Backup (Object : in Fileset_Ref)
                 return Babel.Base.Models.Backup_Ref'Class;

   --  Load the entity identified by 'Id'.
   --  Raises the NOT_FOUND exception if it does not exist.
   procedure Load (Object  : in out Fileset_Ref;
                   Session : in out ADO.Sessions.Session'Class;
                   Id      : in ADO.Identifier);

   --  Load the entity identified by 'Id'.
   --  Returns True in <b>Found</b> if the object was found and False if it does not exist.
   procedure Load (Object  : in out Fileset_Ref;
                   Session : in out ADO.Sessions.Session'Class;
                   Id      : in ADO.Identifier;
                   Found   : out Boolean);

   --  Find and load the entity.
   overriding
   procedure Find (Object  : in out Fileset_Ref;
                   Session : in out ADO.Sessions.Session'Class;
                   Query   : in ADO.SQL.Query'Class;
                   Found   : out Boolean);

   --  Save the entity.  If the entity does not have an identifier, an identifier is allocated
   --  and it is inserted in the table.  Otherwise, only data fields which have been changed
   --  are updated.
   overriding
   procedure Save (Object  : in out Fileset_Ref;
                   Session : in out ADO.Sessions.Master_Session'Class);

   --  Delete the entity.
   overriding
   procedure Delete (Object  : in out Fileset_Ref;
                     Session : in out ADO.Sessions.Master_Session'Class);

   overriding
   function Get_Value (From : in Fileset_Ref;
                       Name : in String) return Util.Beans.Objects.Object;

   --  Table definition
   FILESET_TABLE : constant ADO.Schemas.Class_Mapping_Access;

   --  Internal method to allocate the Object_Record instance
   overriding
   procedure Allocate (Object : in out Fileset_Ref);

   --  Copy of the object.
   procedure Copy (Object : in Fileset_Ref;
                   Into   : in out Fileset_Ref);

   --  Create an object key for Path.
   function Path_Key (Id : in ADO.Identifier) return ADO.Objects.Object_Key;
   --  Create an object key for Path from a string.
   --  Raises Constraint_Error if the string cannot be converted into the object key.
   function Path_Key (Id : in String) return ADO.Objects.Object_Key;

   Null_Path : constant Path_Ref;
   function "=" (Left, Right : Path_Ref'Class) return Boolean;

   --  Set the path identifier.
   procedure Set_Id (Object : in out Path_Ref;
                     Value  : in ADO.Identifier);

   --  Get the path identifier.
   function Get_Id (Object : in Path_Ref)
                 return ADO.Identifier;

   --  Set the file or path name.
   procedure Set_Name (Object : in out Path_Ref;
                       Value  : in Ada.Strings.Unbounded.Unbounded_String);
   procedure Set_Name (Object : in out Path_Ref;
                       Value : in String);

   --  Get the file or path name.
   function Get_Name (Object : in Path_Ref)
                 return Ada.Strings.Unbounded.Unbounded_String;
   function Get_Name (Object : in Path_Ref)
                 return String;

   --
   procedure Set_Store (Object : in out Path_Ref;
                        Value  : in Babel.Base.Models.Store_Ref'Class);

   --
   function Get_Store (Object : in Path_Ref)
                 return Babel.Base.Models.Store_Ref'Class;

   --  Load the entity identified by 'Id'.
   --  Raises the NOT_FOUND exception if it does not exist.
   procedure Load (Object  : in out Path_Ref;
                   Session : in out ADO.Sessions.Session'Class;
                   Id      : in ADO.Identifier);

   --  Load the entity identified by 'Id'.
   --  Returns True in <b>Found</b> if the object was found and False if it does not exist.
   procedure Load (Object  : in out Path_Ref;
                   Session : in out ADO.Sessions.Session'Class;
                   Id      : in ADO.Identifier;
                   Found   : out Boolean);

   --  Find and load the entity.
   overriding
   procedure Find (Object  : in out Path_Ref;
                   Session : in out ADO.Sessions.Session'Class;
                   Query   : in ADO.SQL.Query'Class;
                   Found   : out Boolean);

   --  Save the entity.  If the entity does not have an identifier, an identifier is allocated
   --  and it is inserted in the table.  Otherwise, only data fields which have been changed
   --  are updated.
   overriding
   procedure Save (Object  : in out Path_Ref;
                   Session : in out ADO.Sessions.Master_Session'Class);

   --  Delete the entity.
   overriding
   procedure Delete (Object  : in out Path_Ref;
                     Session : in out ADO.Sessions.Master_Session'Class);

   overriding
   function Get_Value (From : in Path_Ref;
                       Name : in String) return Util.Beans.Objects.Object;

   --  Table definition
   PATH_TABLE : constant ADO.Schemas.Class_Mapping_Access;

   --  Internal method to allocate the Object_Record instance
   overriding
   procedure Allocate (Object : in out Path_Ref);

   --  Copy of the object.
   procedure Copy (Object : in Path_Ref;
                   Into   : in out Path_Ref);

   --  Create an object key for File.
   function File_Key (Id : in ADO.Identifier) return ADO.Objects.Object_Key;
   --  Create an object key for File from a string.
   --  Raises Constraint_Error if the string cannot be converted into the object key.
   function File_Key (Id : in String) return ADO.Objects.Object_Key;

   Null_File : constant File_Ref;
   function "=" (Left, Right : File_Ref'Class) return Boolean;

   --  Set the file identifier.
   procedure Set_Id (Object : in out File_Ref;
                     Value  : in ADO.Identifier);

   --  Get the file identifier.
   function Get_Id (Object : in File_Ref)
                 return ADO.Identifier;

   --  Set the file size.
   procedure Set_Size (Object : in out File_Ref;
                       Value  : in Integer);

   --  Get the file size.
   function Get_Size (Object : in File_Ref)
                 return Integer;

   --  Set the file modification date.
   procedure Set_Date (Object : in out File_Ref;
                       Value  : in Ada.Calendar.Time);

   --  Get the file modification date.
   function Get_Date (Object : in File_Ref)
                 return Ada.Calendar.Time;

   --  Set the file SHA1 signature.
   procedure Set_Sha1 (Object : in out File_Ref;
                       Value  : in ADO.Blob_Ref);

   --  Get the file SHA1 signature.
   function Get_Sha1 (Object : in File_Ref)
                 return ADO.Blob_Ref;

   --
   procedure Set_Directory (Object : in out File_Ref;
                            Value  : in Babel.Base.Models.Path_Ref'Class);

   --
   function Get_Directory (Object : in File_Ref)
                 return Babel.Base.Models.Path_Ref'Class;

   --
   procedure Set_Name (Object : in out File_Ref;
                       Value  : in Babel.Base.Models.Path_Ref'Class);

   --
   function Get_Name (Object : in File_Ref)
                 return Babel.Base.Models.Path_Ref'Class;

   --  Load the entity identified by 'Id'.
   --  Raises the NOT_FOUND exception if it does not exist.
   procedure Load (Object  : in out File_Ref;
                   Session : in out ADO.Sessions.Session'Class;
                   Id      : in ADO.Identifier);

   --  Load the entity identified by 'Id'.
   --  Returns True in <b>Found</b> if the object was found and False if it does not exist.
   procedure Load (Object  : in out File_Ref;
                   Session : in out ADO.Sessions.Session'Class;
                   Id      : in ADO.Identifier;
                   Found   : out Boolean);

   --  Find and load the entity.
   overriding
   procedure Find (Object  : in out File_Ref;
                   Session : in out ADO.Sessions.Session'Class;
                   Query   : in ADO.SQL.Query'Class;
                   Found   : out Boolean);

   --  Save the entity.  If the entity does not have an identifier, an identifier is allocated
   --  and it is inserted in the table.  Otherwise, only data fields which have been changed
   --  are updated.
   overriding
   procedure Save (Object  : in out File_Ref;
                   Session : in out ADO.Sessions.Master_Session'Class);

   --  Delete the entity.
   overriding
   procedure Delete (Object  : in out File_Ref;
                     Session : in out ADO.Sessions.Master_Session'Class);

   overriding
   function Get_Value (From : in File_Ref;
                       Name : in String) return Util.Beans.Objects.Object;

   --  Table definition
   FILE_TABLE : constant ADO.Schemas.Class_Mapping_Access;

   --  Internal method to allocate the Object_Record instance
   overriding
   procedure Allocate (Object : in out File_Ref);

   --  Copy of the object.
   procedure Copy (Object : in File_Ref;
                   Into   : in out File_Ref);




private
   STORE_NAME : aliased constant String := "babel_store";
   COL_0_1_NAME : aliased constant String := "id";
   COL_1_1_NAME : aliased constant String := "name";
   COL_2_1_NAME : aliased constant String := "parameter";
   COL_3_1_NAME : aliased constant String := "server";

   STORE_DEF : aliased constant ADO.Schemas.Class_Mapping :=
     (Count => 4,
      Table => STORE_NAME'Access,
      Members => (
         1 => COL_0_1_NAME'Access,
         2 => COL_1_1_NAME'Access,
         3 => COL_2_1_NAME'Access,
         4 => COL_3_1_NAME'Access
)
     );
   STORE_TABLE : constant ADO.Schemas.Class_Mapping_Access
      := STORE_DEF'Access;

   Null_Store : constant Store_Ref
      := Store_Ref'(ADO.Objects.Object_Ref with others => <>);

   type Store_Impl is
      new ADO.Objects.Object_Record (Key_Type => ADO.Objects.KEY_INTEGER,
                                     Of_Class => STORE_DEF'Access)
   with record
       Name : Ada.Strings.Unbounded.Unbounded_String;
       Parameter : Ada.Strings.Unbounded.Unbounded_String;
       Server : Ada.Strings.Unbounded.Unbounded_String;
   end record;

   type Store_Access is access all Store_Impl;

   overriding
   procedure Destroy (Object : access Store_Impl);

   overriding
   procedure Find (Object  : in out Store_Impl;
                   Session : in out ADO.Sessions.Session'Class;
                   Query   : in ADO.SQL.Query'Class;
                   Found   : out Boolean);

   overriding
   procedure Load (Object  : in out Store_Impl;
                   Session : in out ADO.Sessions.Session'Class);
   procedure Load (Object  : in out Store_Impl;
                   Stmt    : in out ADO.Statements.Query_Statement'Class;
                   Session : in out ADO.Sessions.Session'Class);

   overriding
   procedure Save (Object  : in out Store_Impl;
                   Session : in out ADO.Sessions.Master_Session'Class);

   procedure Create (Object  : in out Store_Impl;
                     Session : in out ADO.Sessions.Master_Session'Class);

   overriding
   procedure Delete (Object  : in out Store_Impl;
                     Session : in out ADO.Sessions.Master_Session'Class);

   procedure Set_Field (Object : in out Store_Ref'Class;
                        Impl   : out Store_Access);
   BACKUP_NAME : aliased constant String := "babel_backup";
   COL_0_2_NAME : aliased constant String := "id";
   COL_1_2_NAME : aliased constant String := "create_date";
   COL_2_2_NAME : aliased constant String := "store_id";

   BACKUP_DEF : aliased constant ADO.Schemas.Class_Mapping :=
     (Count => 3,
      Table => BACKUP_NAME'Access,
      Members => (
         1 => COL_0_2_NAME'Access,
         2 => COL_1_2_NAME'Access,
         3 => COL_2_2_NAME'Access
)
     );
   BACKUP_TABLE : constant ADO.Schemas.Class_Mapping_Access
      := BACKUP_DEF'Access;

   Null_Backup : constant Backup_Ref
      := Backup_Ref'(ADO.Objects.Object_Ref with others => <>);

   type Backup_Impl is
      new ADO.Objects.Object_Record (Key_Type => ADO.Objects.KEY_INTEGER,
                                     Of_Class => BACKUP_DEF'Access)
   with record
       Create_Date : Ada.Calendar.Time;
       Store : Babel.Base.Models.Store_Ref;
   end record;

   type Backup_Access is access all Backup_Impl;

   overriding
   procedure Destroy (Object : access Backup_Impl);

   overriding
   procedure Find (Object  : in out Backup_Impl;
                   Session : in out ADO.Sessions.Session'Class;
                   Query   : in ADO.SQL.Query'Class;
                   Found   : out Boolean);

   overriding
   procedure Load (Object  : in out Backup_Impl;
                   Session : in out ADO.Sessions.Session'Class);
   procedure Load (Object  : in out Backup_Impl;
                   Stmt    : in out ADO.Statements.Query_Statement'Class;
                   Session : in out ADO.Sessions.Session'Class);

   overriding
   procedure Save (Object  : in out Backup_Impl;
                   Session : in out ADO.Sessions.Master_Session'Class);

   procedure Create (Object  : in out Backup_Impl;
                     Session : in out ADO.Sessions.Master_Session'Class);

   overriding
   procedure Delete (Object  : in out Backup_Impl;
                     Session : in out ADO.Sessions.Master_Session'Class);

   procedure Set_Field (Object : in out Backup_Ref'Class;
                        Impl   : out Backup_Access);
   FILESET_NAME : aliased constant String := "babel_fileset";
   COL_0_3_NAME : aliased constant String := "id";
   COL_1_3_NAME : aliased constant String := "first_id";
   COL_2_3_NAME : aliased constant String := "last_id";
   COL_3_3_NAME : aliased constant String := "backup_id";

   FILESET_DEF : aliased constant ADO.Schemas.Class_Mapping :=
     (Count => 4,
      Table => FILESET_NAME'Access,
      Members => (
         1 => COL_0_3_NAME'Access,
         2 => COL_1_3_NAME'Access,
         3 => COL_2_3_NAME'Access,
         4 => COL_3_3_NAME'Access
)
     );
   FILESET_TABLE : constant ADO.Schemas.Class_Mapping_Access
      := FILESET_DEF'Access;

   Null_FileSet : constant FileSet_Ref
      := FileSet_Ref'(ADO.Objects.Object_Ref with others => <>);

   type Fileset_Impl is
      new ADO.Objects.Object_Record (Key_Type => ADO.Objects.KEY_INTEGER,
                                     Of_Class => FILESET_DEF'Access)
   with record
       First_Id : ADO.Identifier;
       Last_Id : ADO.Identifier;
       Backup : Babel.Base.Models.Backup_Ref;
   end record;

   type Fileset_Access is access all Fileset_Impl;

   overriding
   procedure Destroy (Object : access Fileset_Impl);

   overriding
   procedure Find (Object  : in out Fileset_Impl;
                   Session : in out ADO.Sessions.Session'Class;
                   Query   : in ADO.SQL.Query'Class;
                   Found   : out Boolean);

   overriding
   procedure Load (Object  : in out Fileset_Impl;
                   Session : in out ADO.Sessions.Session'Class);
   procedure Load (Object  : in out Fileset_Impl;
                   Stmt    : in out ADO.Statements.Query_Statement'Class;
                   Session : in out ADO.Sessions.Session'Class);

   overriding
   procedure Save (Object  : in out Fileset_Impl;
                   Session : in out ADO.Sessions.Master_Session'Class);

   procedure Create (Object  : in out Fileset_Impl;
                     Session : in out ADO.Sessions.Master_Session'Class);

   overriding
   procedure Delete (Object  : in out Fileset_Impl;
                     Session : in out ADO.Sessions.Master_Session'Class);

   procedure Set_Field (Object : in out Fileset_Ref'Class;
                        Impl   : out Fileset_Access);
   PATH_NAME : aliased constant String := "babel_path";
   COL_0_4_NAME : aliased constant String := "id";
   COL_1_4_NAME : aliased constant String := "name";
   COL_2_4_NAME : aliased constant String := "store_id";

   PATH_DEF : aliased constant ADO.Schemas.Class_Mapping :=
     (Count => 3,
      Table => PATH_NAME'Access,
      Members => (
         1 => COL_0_4_NAME'Access,
         2 => COL_1_4_NAME'Access,
         3 => COL_2_4_NAME'Access
)
     );
   PATH_TABLE : constant ADO.Schemas.Class_Mapping_Access
      := PATH_DEF'Access;

   Null_Path : constant Path_Ref
      := Path_Ref'(ADO.Objects.Object_Ref with others => <>);

   type Path_Impl is
      new ADO.Objects.Object_Record (Key_Type => ADO.Objects.KEY_INTEGER,
                                     Of_Class => PATH_DEF'Access)
   with record
       Name : Ada.Strings.Unbounded.Unbounded_String;
       Store : Babel.Base.Models.Store_Ref;
   end record;

   type Path_Access is access all Path_Impl;

   overriding
   procedure Destroy (Object : access Path_Impl);

   overriding
   procedure Find (Object  : in out Path_Impl;
                   Session : in out ADO.Sessions.Session'Class;
                   Query   : in ADO.SQL.Query'Class;
                   Found   : out Boolean);

   overriding
   procedure Load (Object  : in out Path_Impl;
                   Session : in out ADO.Sessions.Session'Class);
   procedure Load (Object  : in out Path_Impl;
                   Stmt    : in out ADO.Statements.Query_Statement'Class;
                   Session : in out ADO.Sessions.Session'Class);

   overriding
   procedure Save (Object  : in out Path_Impl;
                   Session : in out ADO.Sessions.Master_Session'Class);

   procedure Create (Object  : in out Path_Impl;
                     Session : in out ADO.Sessions.Master_Session'Class);

   overriding
   procedure Delete (Object  : in out Path_Impl;
                     Session : in out ADO.Sessions.Master_Session'Class);

   procedure Set_Field (Object : in out Path_Ref'Class;
                        Impl   : out Path_Access);
   FILE_NAME : aliased constant String := "babel_file";
   COL_0_5_NAME : aliased constant String := "id";
   COL_1_5_NAME : aliased constant String := "size";
   COL_2_5_NAME : aliased constant String := "date";
   COL_3_5_NAME : aliased constant String := "sha1";
   COL_4_5_NAME : aliased constant String := "directory_id";
   COL_5_5_NAME : aliased constant String := "name_id";

   FILE_DEF : aliased constant ADO.Schemas.Class_Mapping :=
     (Count => 6,
      Table => FILE_NAME'Access,
      Members => (
         1 => COL_0_5_NAME'Access,
         2 => COL_1_5_NAME'Access,
         3 => COL_2_5_NAME'Access,
         4 => COL_3_5_NAME'Access,
         5 => COL_4_5_NAME'Access,
         6 => COL_5_5_NAME'Access
)
     );
   FILE_TABLE : constant ADO.Schemas.Class_Mapping_Access
      := FILE_DEF'Access;

   Null_File : constant File_Ref
      := File_Ref'(ADO.Objects.Object_Ref with others => <>);

   type File_Impl is
      new ADO.Objects.Object_Record (Key_Type => ADO.Objects.KEY_INTEGER,
                                     Of_Class => FILE_DEF'Access)
   with record
       Size : Integer;
       Date : Ada.Calendar.Time;
       Sha1 : ADO.Blob_Ref;
       Directory : Babel.Base.Models.Path_Ref;
       Name : Babel.Base.Models.Path_Ref;
   end record;

   type File_Access is access all File_Impl;

   overriding
   procedure Destroy (Object : access File_Impl);

   overriding
   procedure Find (Object  : in out File_Impl;
                   Session : in out ADO.Sessions.Session'Class;
                   Query   : in ADO.SQL.Query'Class;
                   Found   : out Boolean);

   overriding
   procedure Load (Object  : in out File_Impl;
                   Session : in out ADO.Sessions.Session'Class);
   procedure Load (Object  : in out File_Impl;
                   Stmt    : in out ADO.Statements.Query_Statement'Class;
                   Session : in out ADO.Sessions.Session'Class);

   overriding
   procedure Save (Object  : in out File_Impl;
                   Session : in out ADO.Sessions.Master_Session'Class);

   procedure Create (Object  : in out File_Impl;
                     Session : in out ADO.Sessions.Master_Session'Class);

   overriding
   procedure Delete (Object  : in out File_Impl;
                     Session : in out ADO.Sessions.Master_Session'Class);

   procedure Set_Field (Object : in out File_Ref'Class;
                        Impl   : out File_Access);
end Babel.Base.Models;