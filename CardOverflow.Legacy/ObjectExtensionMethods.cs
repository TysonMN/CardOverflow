using System;

namespace CardOverflow.Legacy {
  public static class ObjectExtensionMethods {

    public static TInput Dump<TInput>(this TInput input, string id = "") {
#if DEBUG
      if (string.IsNullOrWhiteSpace(id)) {
        Console.WriteLine(MyObjectDumper.Dump(input));
      } else {
        Console.WriteLine(id + ": " + MyObjectDumper.Dump(input));
      }
#endif
      return input;
    }

    public static TInput Dump<TInput, TDump>(this TInput input, Func<TInput, TDump> getValue, string id = "") {
      getValue(input).Dump(id);
      return input;
    }

  }
}
