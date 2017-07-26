{ pkgs ? import <nixpkgs> { } }:
pkgs.fetchFromGitHub
  {
    owner = "lightandlight";
    repo = "xml-conduit-decode";
    rev = "f37ee691a793f55d1e46188150ca3d5578e9080f";
    sha256 = "12lk8srwdp88vxbbq53ah78zlwm3vbam48flwj5zc3nsyxx3yfw0";
  }
