// $Id: qtest.java,v 1.1 2018-11-22 13:41:43-08 - - $

import static java.lang.System.*;

class qtest {

   public static void main (String[] args) {
      queue<String> queue = new queue<String> ();
      for (String arg : args) queue.insert (arg);
      while (! queue.empty ()) out.println (queue.remove ());
   };

};

