
#!/usr/bin/env bash

cd cells; lein deploy clojars;
cd ../commands; lein deploy clojars;
cd ../editors; lein deploy clojars;
cd ../structure; lein deploy clojars;
cd ../util; lein deploy clojars;
cd ../value-viewer; lein deploy clojars;
cd ../tree; lein deploy clojars;
cd ..;
