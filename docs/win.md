# Compiling on Windows
It appears that `curl`, __but not `curllib`__, is installed by default on Windows10+. Therefore it is necessary to:

1. Download and extract `curl` binaries for windows(https://curl.se/windows/)
2. Compile with
    ```
    stack install --extra-include-dirs=PATH-TO-EXTRACTED-CURL-DIR\include\ --extra-lib-dirs=PATH-TO-EXTRACTED-CURL-DIR\lib
    ```
