# mmmm

Meta MiMiuM. It is a toy compiler to [mimium](https://github.com/mimium-org/mimium).

Its goal is adding object defining/creating notation for now.

## TODO

- [x] parse `./examples/test.mmm`
- [ ] parse [demo.mmm](https://github.com/mimium-org/mimium/blob/dev/examples/demo.mmm) in mimium official examples
- [ ] syntactically same code generation
- [ ] parse object related syntax?
  - [ ] `def objname {slot1: type1, slot2: type2}`
  - [ ] `objname(slot1_value, slot2_value)`
- [ ] object related code generation

## Usage

```sh
git clone https://github.com/t-sin/mmmm
cd mmmm
cargo test
echo 'fn func(a, b) {return a+b}' | cargo run
```

## Author

- t-sin (<shinichi.tanaka45@gmail.com>)

## License

hmm what license should I choose?