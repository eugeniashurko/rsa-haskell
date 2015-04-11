Haskell implementation of RSA encryption algorithm
==================================================

Compile with GHC:

    $ ghc --make Main.hs -o <exec_name>

Commandline usage:

    $ ./<exec_name> help
    $ ./<exec_name> keygen <pubkey_path> <privkey_path>
    $ ./<exec_name> encrypt <pubkey_path> <message_path> <output_path>
    $ ./<exec_name> decrypt <privkey_path> <cypher_path> <output_path>