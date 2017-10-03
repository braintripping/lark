
#!/usr/bin/env bash

cd cells; lein install;
cd ../commands; lein install;
cd ../editors; lein install;
cd ../structure; lein install;
cd ../util; lein install;
cd ../value-viewer; lein install;
cd ../tree; lein install;
cd ..;