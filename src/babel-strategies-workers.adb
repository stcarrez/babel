-----------------------------------------------------------------------
--  babel-strategies-workers -- Tasks that perform strategy work
--  Copyright (C) 2014 Stephane.Carrez
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

with Util.Log.Loggers;
package body Babel.Strategies.Workers is

   Log : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("Babel.Strategies.Workers");

   procedure Configure (Worker  : in out Worker_Type;
                        Process : not null access procedure (S : in out Worker_Strategy)) is
   begin
      for I in Worker.Workers'Range loop
         Process (Worker.Strategies (I));
      end loop;
   end Configure;

   procedure Start (Worker   : in out Worker_Type) is
   begin
      for I in Worker.Workers'Range loop
         Worker.Workers (I).Start (Worker.Strategies (I)'Unchecked_Access);
      end loop;
   end Start;

   procedure Finish (Worker   : in out Worker_Type;
                     Database : in out Babel.Base.Database'Class) is
   begin
      for I in Worker.Workers'Range loop
         Worker.Workers (I).Finish (Database);
      end loop;
   end Finish;

   task body Worker_Task is
      S : Babel.Strategies.Strategy_Type_Access;
   begin
      Log.Info ("Strategy worker is started");
      select
         accept Start (Strategy : in Babel.Strategies.Strategy_Type_Access) do
            S := Strategy;
         end Start;
      or
         terminate;
      end select;
      loop
         begin
            S.Execute;
         exception
            when Babel.Files.Queues.File_Fifo.Timeout =>
               Log.Info ("Strategy worker stopped on timeout");
               exit;

            when E : others =>
               Log.Error ("Strategy worker received exception", E, True);
         end;
      end loop;
      select
         accept Finish (Database : in out Babel.Base.Database'Class) do
            Database.Copy (S.Database);
         end Finish;
      or
         terminate;
      end select;
   end Worker_Task;

end Babel.Strategies.Workers;
