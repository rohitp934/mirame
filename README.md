

# Mirame

Mirame is an interpreted programming language written in Rust. It is designed to follow the principles outlined in Thorsten Ball's book "Writing an Interpreter in Go" and serves as a practical example of how to write an interpreter from scratch using Rust.

## Getting Started

### Prerequisites

- Rust (version 1.55.0 or higher)

### Installing

To install Mirame, clone this repository into your local machine:

```
git clone https://github.com/<your-username>/mirame.git
```

### Running

Once you have cloned the repository, navigate to the root directory of the project and run the following command:

```
cargo run
```

This will start the Mirame interpreter and you can begin executing commands.

### Usage

Mirame currently supports the following commands:

- `let <variable name> = <value>;`: assigns a value to a variable
- `<variable name>;`: retrieves the value of a variable
- `<expression>;`: evaluates an expression

#### Example

```
>> let x = 5;
>> let y = 10;
>> x + y;
15
```

## Contributing

Contributions to Mirame are welcome. If you have any issues or feature requests, please submit them via GitHub issues.

## License

This project is licensed under the MIT License. See the [LICENSE](LICENSE) file for details.
