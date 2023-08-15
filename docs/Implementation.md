# Notes on Platform API injection

As described in [MCTrace.md](../MCTrace.md#current-limitations-of-mctrace), the Platform API implementation is expected
to be provided to the `instrument` command as a single self-contained object file with no external dependencies. Our
current implementation then does the following:
  - Extract the entire `.text` section and insert it in to the binary as a single block. Capture the symbolic address
    we receive from `renovate`.
  - Compute and track the offset (from the base of the text section) of each Platform API function.
  - Any time we need to generate a Platform API call, we use both the symbolic address and offset to generate the calls.

The advantage of our current approach is that, by keeping the entire text section together, we do not alter the relative
locations in code. This allows functions to call each other *as long as* the calls are made via relative addressing of
some form. (Note: If calls are made via a relocation table of some sort, this will *not* work. Getting your
compiler to generate relative calls should not typically be difficult. For example, for both x86-64 and PPC-32, `gcc`
appears to generate relative calls as long as the internal functions being called are `static`).

In general, such an approach works when the text section is fully self-contained; this implies that the Platform API
implementations cannot make use of functions external to the object file or global variables.

*Notes for the future*: extending the implementation to support global variables and indirect function calls via relocation
tables is somewhat involved. For example, to support global variables, we would need to insert the global variable separately
in to the binary and then rewrite the code in the text section to point to the new location in code (possibly utilizing
relocation information in the object file); the exact mechanism needs some investigation. Similarly, supporting multiple,
dependent object files will likely involve some form of "linking" to connect functions/data together during insertion in
to the binary and is likely a fairly complex task given our current tooling.

*Historical Note*: in a previous iteration, instead of injecting the whole `.text` section as a block, we
chose to extract (the bytes of) each relevant Platform API function from the object file and inject it in to the binary
seperately. While straightforward, this had the disadvantage of requiring each function to be fully self-contained and not
make calls to other functions, even within the same module as we had no guarantees with respect to where the code might end
up in the final binary.
