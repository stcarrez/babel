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
private with Ada.Finalization;
private with Ada.Containers.Hashed_Maps;
with Util.Strings;

--  == User database ==
--  The <tt>Babel.Base.Users</tt> package defines a small database that maintains the
--  mapping between a user or group name and the user <tt>uid_t</tt> or group <tt>gid_t</tt>.
--  The mapping is obtained by using the Posix.1-2001 operations getpwnam_r (3) and
--  getgrnam_r (3).  The Posix operations are called once for a given name/id and they are
--  stored in several hash map.  A protected type is used to maintain the user and group
--  mapping cache so that concurrent accesses are possible.
package Babel.Base.Users is

   subtype Name_Access is Util.Strings.Name_Access;

   --  Information about a user and its group.
   type User_Type is record
      Name  : Name_Access;
      Group : Name_Access;
      Uid   : Uid_Type;
      Gid   : Gid_Type;
   end record;

   NO_USER : constant User_Type;

   type Database is tagged limited private;
   type Database_Access is access all Database'Class;

   --  Find the UID and GID for the user that corresponds to the given name and group.
   function Find (From  : in Database;
                  User  : in String;
                  Group : in String) return User_Type;

   --  Find the UID and GID for the user that corresponds to the given name and group.
   function Find (From  : in Database;
                  User  : in Uid_Type;
                  Group : in Gid_Type) return User_Type;

   --  Get the user name associated with the given UID.
   --  Returns null if the UID was not found.
   function Get_Name (From : in Database;
                      Id   : in Uid_Type) return Name_Access;

   --  Get the UID associated with the given user name.
   function Get_Uid (From : in Database;
                     Name : in String) return Uid_Type;

   --  Get the group name associated with the given GID.
   --  Returns null if the GID was not found.
   function Get_Group (From : in Database;
                       Id   : in Gid_Type) return Name_Access;

   --  Get the GID associated with the given group name.
   function Get_Gid (From : in Database;
                     Name : in String) return Gid_Type;

private

   INVALID_ID : constant Natural := Natural'Last;

   NO_USER : constant User_Type := User_Type'(Name  => null,
                                              Group => null,
                                              Uid   => Uid_Type'Last,
                                              Gid   => Gid_Type'Last);

   function Hash (Key : in Natural) return Ada.Containers.Hash_Type;

   package Id_Map is
     new Ada.Containers.Hashed_Maps (Key_Type        => Natural,
                                     Element_Type    => Name_Access,
                                     Hash            => Hash,
                                     Equivalent_Keys => "=",
                                     "="             => Util.Strings."=");

   package Name_Map is
     new Ada.Containers.Hashed_Maps (Key_Type        => Name_Access,
                                     Element_Type    => Natural,
                                     Hash            => Util.Strings.Hash,
                                     Equivalent_Keys => Util.Strings.Equivalent_Keys,
                                     "="             => "=");

   --  To keep the implementation simple, the same protected type and same hash maps
   --  is used for the users and groups database.
   protected type Local_Base is

      --  Find in the ID map for the associated user/group name.
      --  If the item is not found in the map, call the <tt>Search</tt> function to resolve it.
      --  If the ID was not found, return a null name.
      procedure Find_Name (Id     : in Natural;
                           Search : not null access function (Id : in Natural) return String;
                           Name   : out Name_Access);

      --  Find in the name map for the corresponding ID.
      --  If the name was not found in the map, call the <tt>Search</tt> function to resolve it.
      procedure Find_Id (Name   : in String;
                         Search : not null access function (Name : in String) return Natural;
                         Id     : out Natural;
                         Result : out Name_Access);

      procedure Clear;

   private
      Names : Name_Map.Map;
      Ids   : Id_Map.Map;
   end Local_Base;


   type Database is limited new Ada.Finalization.Limited_Controlled with record
      User_Base  : aliased Local_Base;
      Group_Base : aliased Local_Base;
      Users      : access Local_Base;
      Groups     : access Local_Base;
   end record;

   overriding
   procedure Initialize (Db : in out Database);

   overriding
   procedure Finalize (Db : in out Database);

end Babel.Base.Users;
