# Rename the built dist file
mv ./dist/kobold_todomvc_example.js ./dist/kobold_todomvc_example_large.js
# Minimize the dist file into a new one
node_modules/.bin/esbuild --bundle ./dist/kobold_todomvc_example_large.js --minify --outfile=./dist/kobold_todomvc_example.js --format=esm
# Minimize wasm-bindgen import symbols
cargo run --release --package kobold-cli -- dist/kobold_todomvc_example.js
