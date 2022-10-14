use frame_support::traits::GenesisBuild;
use sp_runtime::AccountId32;

use xcm_emulator::{decl_test_network, decl_test_parachain, decl_test_relay_chain};

decl_test_relay_chain! {
	pub struct PolkadotRelay {
		Runtime = polkadot_runtime::Runtime,
		XcmConfig = polkadot_runtime::xcm_config::XcmConfig,
		new_ext = polkadot_ext(),
	}
}

decl_test_parachain! {
	pub struct AcurastParachain {
		Runtime = acurast_runtime::Runtime,
		Origin = acurast_runtime::Origin,
		XcmpMessageHandler = acurast_runtime::XcmpQueue,
		DmpMessageHandler = acurast_runtime::DmpQueue,
		new_ext = acurast_ext(2000),
	}
}

decl_test_parachain! {
	pub struct StatemintParachain {
		Runtime = statemint_runtime::Runtime,
		Origin = statemint_runtime::Origin,
		XcmpMessageHandler = statemint_runtime::XcmpQueue,
		DmpMessageHandler = statemint_runtime::DmpQueue,
		new_ext = statemint_ext(1000),
	}
}

decl_test_network! {
	pub struct Network {
		relay_chain = PolkadotRelay,
		parachains = vec![
			(2000, AcurastParachain),
			(1000, StatemintParachain),
		],
	}
}

pub const ALICE: AccountId32 = AccountId32::new([0u8; 32]);
pub const INITIAL_BALANCE: u128 = 1_000_000_000_000;

pub fn acurast_ext(para_id: u32) -> sp_io::TestExternalities {
	use acurast_runtime::{Runtime, System};

	let mut t = frame_system::GenesisConfig::default()
		.build_storage::<Runtime>()
		.unwrap();

	let parachain_info_config = parachain_info::GenesisConfig {
		parachain_id: para_id.into(),
	};

	<parachain_info::GenesisConfig as GenesisBuild<Runtime, _>>::assimilate_storage(&parachain_info_config, &mut t)
		.unwrap();

	pallet_balances::GenesisConfig::<Runtime> {
		balances: vec![(ALICE, INITIAL_BALANCE)],
	}
	.assimilate_storage(&mut t)
	.unwrap();

	let pallet_xcm_config = pallet_xcm::GenesisConfig::default();
	<pallet_xcm::GenesisConfig as GenesisBuild<Runtime, _>>::assimilate_storage(&pallet_xcm_config, &mut t)
		.unwrap();

	let mut ext = sp_io::TestExternalities::new(t);
	ext.execute_with(|| System::set_block_number(1));
	ext
}

pub fn statemint_ext(para_id: u32) -> sp_io::TestExternalities {
	use statemint_runtime::{Runtime, System};

	let mut t = frame_system::GenesisConfig::default()
		.build_storage::<Runtime>()
		.unwrap();

	let parachain_info_config = parachain_info::GenesisConfig {
		parachain_id: para_id.into(),
	};

	<parachain_info::GenesisConfig as GenesisBuild<Runtime, _>>::assimilate_storage(&parachain_info_config, &mut t)
		.unwrap();

	pallet_balances::GenesisConfig::<Runtime> {
		balances: vec![(ALICE, INITIAL_BALANCE)],
	}
		.assimilate_storage(&mut t)
		.unwrap();

	let pallet_xcm_config = pallet_xcm::GenesisConfig::default();
		<pallet_xcm::GenesisConfig as GenesisBuild<Runtime, _>>::assimilate_storage(&pallet_xcm_config, &mut t)
		.unwrap();

	let mut ext = sp_io::TestExternalities::new(t);
	ext.execute_with(|| System::set_block_number(1));
	ext
}

fn default_parachains_host_configuration(
) -> polkadot_runtime_parachains::configuration::HostConfiguration<polkadot_primitives::v2::BlockNumber> {
	use polkadot_primitives::v2::{MAX_CODE_SIZE, MAX_POV_SIZE};

	polkadot_runtime_parachains::configuration::HostConfiguration {
		minimum_validation_upgrade_delay: 5,
		validation_upgrade_cooldown: 10u32,
		validation_upgrade_delay: 10,
		code_retention_period: 1200,
		max_code_size: MAX_CODE_SIZE,
		max_pov_size: MAX_POV_SIZE,
		max_head_data_size: 32 * 1024,
		group_rotation_frequency: 20,
		chain_availability_period: 4,
		thread_availability_period: 4,
		max_upward_queue_count: 8,
		max_upward_queue_size: 1024 * 1024,
		max_downward_message_size: 1024,
		ump_service_total_weight: 4 * 1_000_000_000,
		max_upward_message_size: 50 * 1024,
		max_upward_message_num_per_candidate: 5,
		hrmp_sender_deposit: 0,
		hrmp_recipient_deposit: 0,
		hrmp_channel_max_capacity: 8,
		hrmp_channel_max_total_size: 8 * 1024,
		hrmp_max_parachain_inbound_channels: 4,
		hrmp_max_parathread_inbound_channels: 4,
		hrmp_channel_max_message_size: 1024 * 1024,
		hrmp_max_parachain_outbound_channels: 4,
		hrmp_max_parathread_outbound_channels: 4,
		hrmp_max_message_num_per_candidate: 5,
		dispute_period: 6,
		no_show_slots: 2,
		n_delay_tranches: 25,
		needed_approvals: 2,
		relay_vrf_modulo_samples: 2,
		zeroth_delay_tranche_width: 0,
		..Default::default()
	}
}

pub fn polkadot_ext() -> sp_io::TestExternalities {
	use polkadot_runtime::{Runtime, System};

	let mut t = frame_system::GenesisConfig::default()
		.build_storage::<Runtime>()
		.unwrap();

	pallet_balances::GenesisConfig::<Runtime> {
		balances: vec![(ALICE, INITIAL_BALANCE)],
	}
	.assimilate_storage(&mut t)
	.unwrap();

	polkadot_runtime_parachains::configuration::GenesisConfig::<Runtime> {
		config: default_parachains_host_configuration(),
	}
	.assimilate_storage(&mut t)
	.unwrap();

	let mut ext = sp_io::TestExternalities::new(t);
	ext.execute_with(|| System::set_block_number(1));
	ext
}

#[cfg(test)]
mod tests {
	use super::*;
	use codec::{Decode, Encode};

	use cumulus_primitives_core::ParaId;
	use frame_support::{assert_ok, RuntimeDebug, traits::Currency};
	use frame_support::dispatch::TypeInfo;
	use sp_runtime::traits::{AccountIdConversion};
	use xcm::{latest::prelude::*};
	// use xcm::v2::{MultiLocation};
	use frame_support::traits::PalletInfoAccess;
	use polkadot_parachain::primitives::Sibling;
	use xcm_emulator::TestExt;
	type AcurastXcmPallet = pallet_xcm::Pallet<acurast_runtime::Runtime>;
	type PolkadotXcmPallet = pallet_xcm::Pallet<polkadot_runtime::Runtime>;
	type StatemintXcmPallet = pallet_xcm::Pallet<statemint_runtime::Runtime>;

	type StatemintMinter = pallet_assets::Pallet<statemint_runtime::Runtime>;
	type AcurastMinter = pallet_assets::Pallet<statemint_runtime::Runtime>;

	#[test]
	fn xcmp_panic() {
		Network::reset();
		let acurast_sovereign: sp_runtime::AccountId32 = Sibling::from(2000).into_account_truncating();
		// create and mint to alice new fungible with id 1
		StatemintParachain::execute_with(|| {
			// create token 1
			let result = StatemintMinter::create(
				statemint_runtime::Origin::signed(ALICE),
				1,
				sp_runtime::MultiAddress::Id(ALICE),
				10
			);
			assert_ok!(result);

			// mint 1500 to alice
			let result = StatemintMinter::mint(
				statemint_runtime::Origin::signed(ALICE),
				1,
				sp_runtime::MultiAddress::Id(ALICE),
				1500
			);
			assert_ok!(result);
			let alice_balance = StatemintMinter::balance(
				1,
				&ALICE,
			);
			assert_eq!(alice_balance, 1500);

			// acurast sovereign account needs a minimum balance of DOT to be a valid account.
			let deposit_result = statemint_runtime::Balances::deposit_creating(
				&acurast_sovereign,
				1_000_000_000_000,
			);
			let acurast_balance = statemint_runtime::Balances::total_balance(&acurast_sovereign);
			assert_eq!(acurast_balance, 1_000_000_000_000);

			// mint 1500 of token 1 to acurast sovereign acc
			let result = StatemintMinter::mint(
				statemint_runtime::Origin::signed(ALICE),
				1,
				sp_runtime::MultiAddress::Id(acurast_sovereign.clone()),
				1500
			);
			assert_ok!(result);
			let acurast_token_balance = StatemintMinter::balance(
				1,
				&acurast_sovereign,
			);
			assert_eq!(acurast_token_balance, 1500);
		});

		// create same asset in acurast
		AcurastParachain::execute_with(|| {
			let result = AcurastMinter::create(
				statemint_runtime::Origin::signed(ALICE),
				1,
				sp_runtime::MultiAddress::Id(ALICE),
				10
			);
			assert_ok!(result);
		});

		// reserve backed transfer of token 1 from statemint to acurast
		StatemintParachain::execute_with(|| {
			let xcm = StatemintXcmPallet::reserve_transfer_assets(
				statemint_runtime::Origin::signed(ALICE),
				Box::new(MultiLocation { parents: 1, interior: X1(Parachain(2000)) }.into()),
				Box::new(X1(Junction::AccountId32 {
					network: NetworkId::Any,
					id: ALICE.into()
				}).into().into()),
				Box::new(vec![
					MultiAsset {
						id: Concrete(X2(PalletInstance(50), GeneralIndex(1)).into()),
						fun: Fungible(500)
					},
					MultiAsset {
						id: Concrete(Parent.into()),
						fun: Fungible(INITIAL_BALANCE / 10)
					},
				].into()),
				1,
			);
			assert_ok!(xcm);
		});

		StatemintParachain::execute_with(|| {
			let _events = statemint_runtime::System::events();
			println!("stop");
		});

		AcurastParachain::execute_with(|| {
			let _events = acurast_runtime::System::events();
			let alice_balance = AcurastMinter::balance(1, &ALICE);
			assert_eq!(alice_balance, 500);
		});
	}

	#[test]
	fn dmp() {
		Network::reset();

		let remark = acurast_runtime::Call::System(frame_system::Call::<acurast_runtime::Runtime>::remark_with_event {
			remark: "Hello from Atera".as_bytes().to_vec(),
		});
		PolkadotRelay::execute_with(|| {
			assert_ok!(polkadot_runtime::XcmPallet::force_default_xcm_version(
				polkadot_runtime::Origin::root(),
				Some(0)
			));
			assert_ok!(polkadot_runtime::XcmPallet::send_xcm(
				Here,
				Parachain(2000),
				Xcm(vec![Transact {
					origin_type: OriginKind::SovereignAccount,
					require_weight_at_most: INITIAL_BALANCE as u64,
					call: remark.encode().into(),
				}]),
			));
		});

		AcurastParachain::execute_with(|| {
			use acurast_runtime::{Event, System};
			System::events().iter().for_each(|r| println!(">>> {:?}", r.event));

			assert!(System::events().iter().any(|r| matches!(
				r.event,
				Event::System(frame_system::Event::Remarked { sender: _, hash: _ })
			)));
		});
	}

	#[test]
	fn ump() {
		Network::reset();

		PolkadotRelay::execute_with(|| {
			let _ = polkadot_runtime::Balances::deposit_creating(
				&ParaId::from(2000).into_account_truncating(),
				1_000_000_000_000,
			);
		});

		let remark = polkadot_runtime::Call::System(frame_system::Call::<polkadot_runtime::Runtime>::remark_with_event {
			remark: "Hello from Acurast!".as_bytes().to_vec(),
		});

		let send_amount = 1_000_000_000_000;
		AcurastParachain::execute_with(|| {
			assert_ok!(acurast_runtime::PolkadotXcm::send_xcm(
				Here,
				Parent,
				Xcm(vec![
						WithdrawAsset((Here, send_amount).into()),
						buy_execution((Here, send_amount)),
						Transact {
							origin_type: OriginKind::SovereignAccount,
							require_weight_at_most: INITIAL_BALANCE as u64,
							call: remark.encode().into(),
						}
				]),
			));
		});

		PolkadotRelay::execute_with(|| {
			use polkadot_runtime::{Event, System};
			let _event_list = System::events();
			assert!(System::events().iter().any(|r| matches!(
				r.event,
				Event::System(frame_system::Event::Remarked { sender: _, hash: _ })
			)));
		});
	}


	#[test]
	#[should_panic] // because xcm execution is not paid. Barrier on xcm_executor::Config stops it
	fn xcmp() {
		Network::reset();

		let remark = statemint_runtime::Call::System(frame_system::Call::<statemint_runtime::Runtime>::remark_with_event {
			remark: "Hello from acurast!".as_bytes().to_vec(),
		});
		AcurastParachain::execute_with(|| {
			assert_ok!(acurast_runtime::PolkadotXcm::send_xcm(
				Here,
				MultiLocation::new(1, X1(Parachain(1000))),
				Xcm(vec![Transact {
					origin_type: OriginKind::SovereignAccount,
					require_weight_at_most: 10_000_000,
					call: remark.encode().into(),
				}]),
			));
		});

		StatemintParachain::execute_with(|| {
			use statemint_runtime::{Event, System};
			let evs = System::events();
			System::events().iter().for_each(|r| println!(">>> {:?}", r.event));

			assert!(System::events().iter().any(|r| matches!(
				r.event,
				Event::System(frame_system::Event::Remarked { sender: _, hash: _ })
			)));
		});
	}

	// individual holding assets in their statemint chain account, sends a reserve transfer of DOT
	// to an individual on the acurast chain
	#[test]
	fn dot_reserve_transfer() {
		Network::reset();
		StatemintParachain::execute_with(|| {
			let _alice_balance = pallet_balances::Pallet::<statemint_runtime::Runtime>::free_balance(&ALICE);
			let xcm = StatemintXcmPallet::reserve_transfer_assets(
				statemint_runtime::Origin::signed(ALICE),
				Box::new(MultiLocation{parents: 1, interior: X1(Parachain(2000))}.into()),
				Box::new(X1(Junction::AccountId32 {
					network: NetworkId::Any,
					id: ALICE.into()
				}).into().into()),
				Box::new(vec![
					MultiAsset {
						id: Concrete(Parent.into()),
						fun: Fungible(INITIAL_BALANCE/2)
					},
				].into()),
				0,
			);
			assert_ok!(xcm);
		});

		StatemintParachain::execute_with(|| {
			let _events = statemint_runtime::System::events();
			println!("events!")
		});

		AcurastParachain::execute_with(|| {
			let _events = acurast_runtime::System::events();
			println!("events!")
		});


		AcurastParachain::execute_with(|| {
			let alice_balance = pallet_balances::Pallet::<acurast_runtime::Runtime>::free_balance(&ALICE);
			assert!(alice_balance < INITIAL_BALANCE + INITIAL_BALANCE/2 && alice_balance > INITIAL_BALANCE)
		})
	}

	#[test]
	fn mint_new_fungible() {
		Network::reset();
		StatemintParachain::execute_with(|| {
			let result = StatemintMinter::create(
				statemint_runtime::Origin::signed(ALICE),
				1,
				sp_runtime::MultiAddress::Id(ALICE),
				10
			);
			assert_ok!(result);

			let result = StatemintMinter::mint(
				statemint_runtime::Origin::signed(ALICE),
				1,
				sp_runtime::MultiAddress::Id(ALICE),
				1500
			);
			assert_ok!(result);

			let alice_balance = StatemintMinter::balance(
				1,
				&ALICE,
			);

			assert_eq!(alice_balance, 1500);
		})
	}

	// Helper function for forming buy execution message
	fn buy_execution<C>(fees: impl Into<MultiAsset>) -> Instruction<C> {
		BuyExecution { fees: fees.into(), weight_limit: Unlimited }
	}
}