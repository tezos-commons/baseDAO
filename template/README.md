# DAO template

This package serves as a template for writing new DAO contracts.

## How to write your own DAOs

To create your own DAO contract:
1. Either fork this repository, or copy the [`template`](/template) folder to your repository;
2. Fill the package information in [`package.yaml`](./package.yaml) and [stack project config](./stack.yaml);
3. Fill the [contract template module](./app/Main.hs) with your own implementation;
4. After building the project, the produced executable can serve your contract, use `stack exec <exec-name>` to run it.
