Staking-Fixed-APY-STX
A staking smart contract built with Clarity on the Stacks blockchain.
Users can stake STX tokens to earn rewards at a fixed annual percentage yield (APY).

Features
Stake STX tokens to earn passive income
Fixed APY reward mode
Claim accumulated rewards
Unstake after lock period
Transparent event logs

Technical Overview
Language: Clarity
Core Functions:
stake – deposit STX into the staking pool
claim-reward – claim earned rewards
unstake – withdraw staked STX after lock period
get-stake-info – view staking details for an account

Installation & Usage
Clone repository:
git clone https://github.com/your-repo/staking-fixed-apy-stx.git
cd staking-fixed-apy-stx

Deploy with Clarinet:
clarinet contract deploy staking-fixed-apy-stx

Run tests:
clarinet test

Roadmap
Add multiple staking pools with different APYs
Support for SIP-010 fungible tokens
Enable auto-compounding rewards
Security review & optimization

License
MIT License – free to use, modify, and distribute.
