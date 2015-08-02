-----------------------------------------------------------------------
--  babel-base-users-- User's database for file owership identification
--  Copyright (C) 2014, 2015 Stephane.Carrez
--  Written by Stephane.Carrez (Stephane.Carrez@gmail.com)
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
with System;
with Interfaces.C.Strings;

package body Babel.Base.Users is

   use type Interfaces.C.int;
   use type Interfaces.C.Strings.chars_ptr;

   type Passwd is record
      pw_name   : Interfaces.C.Strings.chars_ptr;
      pw_passwd : Interfaces.C.Strings.chars_ptr;
      pw_uid    : Util.Systems.Types.uid_t;
      pw_gid    : Util.Systems.Types.gid_t;
      pw_gecos  : Interfaces.C.Strings.chars_ptr;
      pw_dir    : Interfaces.C.Strings.chars_ptr;
      pw_shell  : Interfaces.C.Strings.chars_ptr;
   end record;
   pragma Convention (C, Passwd);

   type Passwd_Access is access all Passwd;

   type Group is record
      gr_name   : Interfaces.C.Strings.chars_ptr;
      gr_passwd : Interfaces.C.Strings.chars_ptr;
      gr_gid    : Util.Systems.Types.gid_t;
      gr_mem    : Interfaces.C.Strings.chars_ptr;
   end record;
   pragma Convention (C, Group);

   type Group_Access is access all Group;

   --  Get the user name that correspond to the given user ID (looking at /etc/passwd).
   function Get_User (Id : in Natural) return String;

   --  Get the user UID that corresponds to the given user name (looking at /etc/passwd).
   function Get_User (Name : in String) return Natural;

   --  Get the group name that correspond to the given group ID (looking at /etc/group).
   function Get_Group (Id : in Natural) return String;

   --  Get the group UID that corresponds to the given group name (looking at /etc/group).
   function Get_Group (Name : in String) return Natural;

   --  ------------------------------
   --  Get the user name that correspond to the given user ID (looking at /etc/passwd).
   --  ------------------------------
   function Get_User (Id : in Natural) return String is
      use Util.Systems.Types;

      function getpwuid_r (Id     : Util.Systems.Types.uid_t;
                           Pwd    : Passwd_Access;
                           Buf    : System.Address;
                           Buflen : Interfaces.C.size_t;
                           Res    : System.Address) return Interfaces.C.int;
      pragma Import (C, getpwuid_r, "getpwuid_r");

      Pwd     : aliased Passwd;
      Pwd_Res : aliased Passwd_Access;
      Buf     : aliased String (1 .. 4096);
      Res     : Interfaces.C.int;
   begin
      Res := getpwuid_r (uid_t (Id), Pwd'Unchecked_Access, Buf (1)'Address, Buf'Length,
                         Pwd_Res'Address);
      if Res /= 0 or else Pwd_Res = null or else Pwd.pw_name = Interfaces.C.Strings.Null_Ptr then
         return "";
      end if;
      return Interfaces.C.Strings.Value (Pwd.pw_name);
   end Get_User;

   --  ------------------------------
   --  Get the user UID that corresponds to the given user name (looking at /etc/passwd).
   --  ------------------------------
   function Get_User (Name : in String) return Natural is
      use Util.Systems.Types;

      function getpwnam_r (Name   : Interfaces.C.Strings.chars_ptr;
                           Pwd    : Passwd_Access;
                           Buf    : System.Address;
                           Buflen : Interfaces.C.size_t;
                           Res    : System.Address) return Interfaces.C.int;
      pragma Import (C, getpwnam_r, "getpwnam_r");

      Pwd     : aliased Passwd;
      Pwd_Res : aliased Passwd_Access;
      Buf     : aliased String (1 .. 4096);
      Res     : Interfaces.C.int;
      Ptr     : Interfaces.C.Strings.chars_ptr := Interfaces.C.Strings.New_String (Name);
   begin
      Res := getpwnam_r (Ptr, Pwd'Unchecked_Access, Buf (1)'Address, Buf'Length,
                         Pwd_Res'Address);
      Interfaces.C.Strings.Free (Ptr);
      if Res /= 0 or else Pwd_Res = null or else Pwd.pw_name = Interfaces.C.Strings.Null_Ptr then
         return INVALID_ID;
      end if;
      return Natural (Pwd.pw_uid);
   end Get_User;

   --  ------------------------------
   --  Get the group name that correspond to the given group ID (looking at /etc/group).
   --  ------------------------------
   function Get_Group (Id : in Natural) return String is
      use Util.Systems.Types;

      function getgrgid_r (Id     : Util.Systems.Types.uid_t;
                           Grp    : Group_Access;
                           Buf    : System.Address;
                           Buflen : Interfaces.C.size_t;
                           Res    : System.Address) return Interfaces.C.int;
      pragma Import (C, getgrgid_r, "getgrgid_r");

      Grp     : aliased Group;
      Grp_Res : aliased Group_Access;
      Buf     : aliased String (1 .. 4096);
      Res     : Interfaces.C.int;
   begin
      Res := getgrgid_r (gid_t (Id), Grp'Unchecked_Access, Buf (1)'Address, Buf'Length,
                         Grp_Res'Address);
      if Res /= 0 or else Grp_Res = null or else Grp.gr_name = Interfaces.C.Strings.Null_Ptr then
         return "";
      end if;
      return Interfaces.C.Strings.Value (Grp.gr_name);
   end Get_Group;

   --  ------------------------------
   --  Get the group UID that corresponds to the given group name (looking at /etc/group).
   --  ------------------------------
   function Get_Group (Name : in String) return Natural is
      use Util.Systems.Types;

      function getgrnam_r (Name   : Interfaces.C.Strings.chars_ptr;
                           Grp    : Group_Access;
                           Buf    : System.Address;
                           Buflen : Interfaces.C.size_t;
                           Res    : System.Address) return Interfaces.C.int;
      pragma Import (C, getgrnam_r, "getgrnam_r");

      Grp     : aliased Group;
      Grp_Res : aliased Group_Access;
      Buf     : aliased String (1 .. 4096);
      Res     : Interfaces.C.int;
      Ptr     : Interfaces.C.Strings.chars_ptr := Interfaces.C.Strings.New_String (Name);
   begin
      Res := getgrnam_r (Ptr, Grp'Unchecked_Access, Buf (1)'Address, Buf'Length,
                         Grp_Res'Address);
      Interfaces.C.Strings.Free (Ptr);
      if Res /= 0 or else Grp_Res = null or else Grp.gr_name = Interfaces.C.Strings.Null_Ptr then
         return INVALID_ID;
      end if;
      return Natural (Grp.gr_gid);
   end Get_Group;

   --  ------------------------------
   --  Find the UID and GID for the user that corresponds to the given name and group.
   --  ------------------------------
   function Find (From  : in Database;
                  User  : in String;
                  Group : in String) return User_Type is
      Result : User_Type;
      Id     : Natural;
   begin
      From.Users.Find_Id (User, Get_User'Access, Id, Result.Name);
      Result.Uid := Uid_Type (Id);
      From.Groups.Find_Id (Group, Get_Group'Access, Id, Result.Group);
      Result.Gid := Gid_Type (Id);
      return Result;
   end Find;

   --  ------------------------------
   --  Find the UID and GID for the user that corresponds to the given name and group.
   --  ------------------------------
   function Find (From  : in Database;
                  User  : in Uid_Type;
                  Group : in Gid_Type) return User_Type is
      Result : User_Type;
   begin
      Result.Uid := User;
      Result.Gid := Group;
      From.Users.Find_Name (Natural (User), Get_User'Access, Result.Name);
      From.Groups.Find_Name (Natural (Group), Get_Group'Access, Result.Group);
      return Result;
   end Find;

   --  ------------------------------
   --  Get the user name associated with the given UID.
   --  Returns null if the UID was not found.
   --  ------------------------------
   function Get_Name (From : in Database;
                      Id   : in Uid_Type) return Name_Access is
      Result : Name_Access;
   begin
      From.Users.Find_Name (Natural (Id), Get_User'Access, Result);
      return Result;
   end Get_Name;

   --  ------------------------------
   --  Get the UID associated with the given user name.
   --  ------------------------------
   function Get_Uid (From : in Database;
                     Name : in String) return Uid_Type is
      Uid         : Natural;
      Result_Name : Name_Access;
   begin
      From.Users.Find_Id (Name, Get_User'Access, Uid, Result_Name);
      return Uid_Type (Uid);
   end Get_Uid;

   --  ------------------------------
   --  Get the group name associated with the given GID.
   --  Returns null if the GID was not found.
   --  ------------------------------
   function Get_Group (From : in Database;
                       Id   : in Gid_Type) return Name_Access is
         Result : Name_Access;
   begin
      From.Groups.Find_Name (Natural (Id), Get_Group'Access, Result);
      return Result;
   end Get_Group;

   --  ------------------------------
   --  Get the GID associated with the given group name.
   --  ------------------------------
   function Get_Gid (From : in Database;
                     Name : in String) return Gid_Type is
      Gid         : Natural;
      Result_Name : Name_Access;
   begin
      From.Groups.Find_Id (Name, Get_Group'Access, Gid, Result_Name);
      return Gid_Type (Gid);
   end Get_Gid;


   function Hash (Key : in Natural) return Ada.Containers.Hash_Type is
   begin
      return Ada.Containers.Hash_Type (Key);
   end Hash;

   --  To keep the implementation simple, the same protected type and same hash maps
   --  is used for the users and groups database.
   protected body Local_Base is

      --  ------------------------------
      --  Find in the ID map for the associated user/group name.
      --  If the item is not found in the map, call the <tt>Search</tt> function to resolve it.
      --  If the ID was not found, return a null name.
      --  ------------------------------
      procedure Find_Name (Id     : in Natural;
                           Search : not null access function (Id : in Natural) return String;
                           Name   : out Name_Access) is
         Pos : constant Id_Map.Cursor := Ids.Find (Id);
      begin
         if Id_Map.Has_Element (Pos) then
            Name := Id_Map.Element (Pos);
         else
            declare
               N : constant String := Search (Id);
            begin
               if N'Length = 0 then
                  Name := null;
               else
                  Name := new String'(N);
                  Ids.Insert (Id, Name);
                  Names.Insert (Name, Id);
               end if;
            end;
         end if;
      end Find_Name;

      --  ------------------------------
      --  Find in the name map for the corresponding ID.
      --  If the name was not found in the map, call the <tt>Search</tt> function to resolve it.
      --  ------------------------------
      procedure Find_Id (Name   : in String;
                         Search : not null access function (Name : in String) return Natural;
                         Id     : out Natural;
                         Result : out Name_Access) is
         Pos : constant Name_Map.Cursor := Names.Find (Name'Unrestricted_Access);
      begin
         if Name_Map.Has_Element (Pos) then
            Id := Name_Map.Element (Pos);
            Result := Name_Map.Key (Pos);
         else
            Id := Search (Name);
            if Id /= INVALID_ID then
               declare
                  N : constant Name_Access := new String '(Name);
               begin
                  Result := N;
                  Ids.Insert (Id, N);
                  Names.Insert (N, Id);
               end;
            else
               Result := null;
            end if;
         end if;
      end Find_Id;

      procedure Clear is
      begin
         --  @todo: release the storage allocated for names.
         null;
      end Clear;

   end Local_Base;

   overriding
   procedure Initialize (Db : in out Database) is
   begin
      Db.Users  := Db.User_Base'Unchecked_Access;
      Db.Groups := Db.Group_Base'Unchecked_Access;
   end Initialize;

   overriding
   procedure Finalize (Db : in out Database) is
   begin
      Db.User_Base.Clear;
      Db.Group_Base.Clear;
   end Finalize;

end Babel.Base.Users;
