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

   procedure Start (Worker   : in out Worker_Type;
                    Strategy : in Babel.Strategies.Strategy_Type_Access) is
   begin
      for I in Worker.Workers'Range loop
         Worker.Workers (I).Start (Strategy);
      end loop;
   end Start;

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
            when Babel.Files.File_Fifo.Timeout =>
               Log.Info ("Strategy worker stopped on timeout");
               exit;

            when E : others =>
               Log.Error ("Strategy worker received exception", E);
         end;
      end loop;
   end Worker_Task;

end Babel.Strategies.Workers;
