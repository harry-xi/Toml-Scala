# Toml-Util

A simple toml deserialization tools for scala3.

## Notice

It allows mapping toml integers to scala.Int and toml decimals to scala.Float, but it should be noted that the toml specification requires implementations to use the 64-bit format for decimals and integers.

For BigDecimal, we implement it as a type corresponding to toml strings.

## Acknowledgments

This project is in fact a toml port of the json deserialization module from [wz7982/sqala](https://github.com/wz7982/sqala), without which this project would not have been born.

Thanks also to [indoorvivants/toml-scala](https://github.com/indoorvivants/toml-scala), on which this project depends for parsing the toml and generating the AST.
