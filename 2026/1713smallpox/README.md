# 2026/1713smallpox (version 2, revised manuscript)

Replication materials for:

> Fourie, J. (2026). *A disease never seen here: Smallpox and family reconstruction at the Cape, 1713–1714.* Revised manuscript, *The History of the Family*.

**This version replaces the July 2026 release** that accompanied the submitted manuscript (*Measuring the Severity of the 1713 Smallpox Epidemic at the Cape*). The submitted version's validated-classifier estimates, corrected journal shares, group shares, and mortality-comparison table are withdrawn. In particular, the submitted manuscript's statement that two human readers agreed with the reference labels on all 79 re-coded entries was incorrect; `data/validation/validation_agreement.csv` reports the actual comparisons.

The package is aggregate-level. Individual-level opgaaf rows and probate registers, which carry names, are not redistributed. The underlying transcriptions remain governed by their custodians (see *Sources*).

## Contents

```
data/census/       recorded tax-unit stocks by district and year (census_annual.csv), two-year changes,
                   adult-proportional child benchmark, missing-cell counts, the enslaved-girls ('Meijsies')
                   mapping ledger and the release reconciliation
data/probate/      Orphan Chamber (MOOC8) documents per heading year, 1695-1720, and baseline ratios
data/journal/      full-corpus lexical counts per year (1700-1720), the 18 smallpox candidates with decisions,
                   the 17-entry chronology, 94 medical candidates, care-context register, image concordance,
                   retrieval protocol (exact expressions) and summary results
data/company/      the Company stock-flow ledger from the 26 August 1713 journal entry
data/validation/   hand-coded sample compared with the reference labels
figures/           Figures 1-3 of the article and Figure A1 of the supplement (PDF and PNG)
scripts/           aggregate reproduction runner plus the original source-extraction scripts
```

## Reproduce

Run `python -X utf8 scripts/run_revision.py` from any directory. The existing runner reads only the deposited CSVs and regenerates all seven numerical LaTeX tables and all four figures (PDF and PNG) under `reproduced/tables/` and `reproduced/figures/`. Use `--output-dir PATH` to choose another destination. The qualitative mechanism and evidence-summary tables are authored text rather than calculated outputs. Python 3.13 was used; the aggregate runner requires pandas and matplotlib (versions in `scripts/requirements.txt`). It requires neither raw transcriptions, a model API, Pandoc nor LaTeX.

This reproduces the exhibits from the deposited aggregates; it does not independently reconstruct those aggregates from the historical sources. The numbered scripts retain the original source-extraction and author-packaging workflow for inspection. They depend on the author's `THoTF_revision/analysis`, `outputs`, `sources`, `validation`, `full_corpus`, `manuscript`, `response`, `logs` and `replication` directories, the original THoTF coding files, the sibling Smallpox `sources/data` directory, and the Households `Data/v1` release. They are not standalone entry points in this public directory. The author workspace's separate `replication/run_revision.py` runs that complete local pipeline.

The source-input hashes below identify the versions used. The complete journal-entry ledger, detailed estate-document audit, individual tax-unit data and original validation workbooks remain in the author's research files; access and redistribution are subject to their custodians' terms. Public candidate and care registers, aggregate validation comparisons and source locators are included here. The article and supplement are built from the separate manuscript LaTeX submission package, not by this aggregate runner.

Headline values: 1713 probate documents 54 (1714: 36; 1708–1712 mean 10.8). Recorded settlers, Cape District and Stellenbosch–Drakenstein combined, 1,967 (1712) → 1,488 (1714). Privately enslaved children 143 → 108 (Cape) and 89 → 68 (Stellenbosch–Drakenstein). Journal: 7,670 daily identifiers, 7,666 usable texts; 18 smallpox candidates, 17 retained (16 in 1713, one in February 1714).

## Sources

- **Tax returns (*opgaafrolle*)**: Cape of Good Hope Panel transcriptions (Fourie & Green, 2018, *The History of the Family* 23(3), 493–502; Fourie et al., 2024, *South African Historical Journal* 76(4)); archival originals NA, VOC 4068 and 4073.
- **Estate papers (MOOC8)**: transcribed by the TEPC Transcription Project (2004–2008) at the Western Cape Archives and Records Service; public rendering by GLOBALISE (TANAP Resources).
- **Daily journal (*dagregister*)**: transcription by the Tracing History Trust, shared by Helena Liebenberg; originals NA, VOC 1.04.02, inv. 10730–10733.

## Source input versions

SHA-256 values recorded for the 83 source files used in the local extraction. Paths are relative to the Smallpox workspace except for the explicitly identified sibling Households project. The two validation workbooks are retained research inputs, not public files.

| Source file | SHA-256 |
|---|---|
| `sources/data/XML files/MOOC8_1.01-85_v3.1.00.xml` | `3d51a73b7c3ce5e6edb8ec2c74bfd352a21b458aab2bbf18c4f613f846ffba98` |
| `sources/data/XML files/MOOC8_10.01-76_v3.2.00.xml` | `5dab2becd41de6d919451cb867dad38a63559e69f300b0d7b3f1df7b8172da77` |
| `sources/data/XML files/MOOC8_11.01-46_v3.2.00.xml` | `1ab8ed7db1d85609570885bb82088ae92d636c12fe4aad31a50883cbcbd75bf7` |
| `sources/data/XML files/MOOC8_12.01-64_v3.2.00.xml` | `258601418fb30080f6aea7b144621100c48d90c55870f50deee0196f9122cbdc` |
| `sources/data/XML files/MOOC8_13.01-70_v3.2.00.xml` | `c38a0cc609eee7f11ac9e6353eacfe3fcbb2747e96c10b3f20aa440ab2e94d07` |
| `sources/data/XML files/MOOC8_14.01-66_v3.2.00.xml` | `8049895eeaff5ec5187146a153a156e5bd74e3ef0cc497f924d9d25626f02e67` |
| `sources/data/XML files/MOOC8_15.01-47_v3.2.00.xml` | `8b302ac4ccd80afd5212db1ef1aafe90fd7a0ead7899003f126d16a170c16ec9` |
| `sources/data/XML files/MOOC8_16.01-60_v3.2.00.xml` | `865f8110a7884df911b71836b4feb7db38da3ba8499dab707370f4afc27f1b73` |
| `sources/data/XML files/MOOC8_17.01-76_v3.2.00.xml` | `7d28e18a184360713e8b1d99fb455624ba3d1de7ec0683168f9461f3f539e3dd` |
| `sources/data/XML files/MOOC8_18.01-102_v3.2.00.xml` | `6538cdb2e23903b6b0186a1d924b5b337e782da969001a731060b703a2e627a9` |
| `sources/data/XML files/MOOC8_19.01-80_v3.2.00.xml` | `2ac8b473be5533b14b512b251fb94adae2af7cafeedfa77beab2ed2966bd8f79` |
| `sources/data/XML files/MOOC8_2.01-122_v3.1.00.xml` | `234c680d986eb41277b2bf7426f1618e3fdd8dc23283cdbb891c8f1ab7eb68ed` |
| `sources/data/XML files/MOOC8_20.01-52_v3.2.00.xml` | `8b69b392699e1fc6dd817eba91a965b868d29d94c6487efe4f7c2cb1f279a41b` |
| `sources/data/XML files/MOOC8_21.01-57_v3.2.00.xml` | `408d212fd275730a010bebb371fb1cdd763df81f6c1a08ddd6224ca3df7b741c` |
| `sources/data/XML files/MOOC8_22.01-57_v3.2.00.xml` | `38ac724eadde49575f3b189098491862cc93ae4008f0c0a37897a2fd13a58844` |
| `sources/data/XML files/MOOC8_23.01-53_v3.2.00.xml` | `acca30a51f1c4af063dea099e17a21733e2db33ed79933b60d5e635fae74a734` |
| `sources/data/XML files/MOOC8_24.01-68_v3.2.00.xml` | `90db9453edb86510492fa507a989c45ea3e8ad586d3f25400ee141fe0a17ca87` |
| `sources/data/XML files/MOOC8_25.01-46_v3.2.00.xml` | `b31a6bdaf5e4c6aeab504903ab2f141aca436d97d197c36cb6ec82916a447a7c` |
| `sources/data/XML files/MOOC8_26.01-73_v3.2.00.xml` | `e4f5c98a689a2d41950bda8e3dda2fbe9fcd4e08bb7c30c72fcf3869067dced1` |
| `sources/data/XML files/MOOC8_27.01-46_v3.2.00.xml` | `ddece17732ed71f962c712631f4e962eefa5728f42ad4d131f07f5a928f76450` |
| `sources/data/XML files/MOOC8_28.01-72_v3.2.00.xml` | `02b7c67bca6dfd26d6571e79f178b8a74a0f4300d99b57cc1a9d125b6bb1c26a` |
| `sources/data/XML files/MOOC8_29.01-44_v3.2.00.xml` | `46d94f92b8c730a8e5a45ec2e039dd494d60f8f20a6c9f4dba0b5395d6c9bb21` |
| `sources/data/XML files/MOOC8_3.01-107_v3.1.00.xml` | `1840f0c18da38a3f605ca3e1fca855f6c75d2946fe2dc6d1870d997a707b25bc` |
| `sources/data/XML files/MOOC8_30.01-70_v3.2.00.xml` | `ca91e6faaf138e4a6c3842e3c8e127afe8fae5848e120ba3e4da3955a91c6506` |
| `sources/data/XML files/MOOC8_31.01-76_v3.3.00.xml` | `a83af31e9e6071720a42496f733ea3d48e7f684ff8ae515c75c4e4c2012991ae` |
| `sources/data/XML files/MOOC8_32.01-52_v3.2.00.xml` | `8bb61754b9b17b53c94c6b716b01082ba77b8d6e1ad8c00eed13517accd5b8e8` |
| `sources/data/XML files/MOOC8_33.01-61_v3.2.00.xml` | `4bd7d46113c9a3bc9b0ac03e0c741b1faa9ba297879bacda92f6bf7fc3a53640` |
| `sources/data/XML files/MOOC8_34.01-52_v3.2.00.xml` | `ce891bf718166f1ac4b3ea15f271fe94239ede16b4487c6d20b7092104f11519` |
| `sources/data/XML files/MOOC8_35.01-37_v3.2.00.xml` | `ee4d271944f9c93ce0938d15393bd3794bdc516e011d539484895b973f6cdb08` |
| `sources/data/XML files/MOOC8_36.01-75_v3.2.00.xml` | `b308c7647ab847cb779ab4aacfdeb2c8d08005e159f61e5f3bc9e0682d2be86e` |
| `sources/data/XML files/MOOC8_37.01-55_v3.2.00.xml` | `6a7b7a895f2e48565995a08d8572c6ee0197c93a2147e1e96ef1e8bd543f3df7` |
| `sources/data/XML files/MOOC8_38.01-65_v3.2.00.xml` | `ccf5749d1da12606655a09c2e0714b8c6360ba32919db77c616830bc4b446131` |
| `sources/data/XML files/MOOC8_39.01-53_v3.2.00.xml` | `b3f7836ed0e09a5f26e6eb978184e5e05c7971aee56dd12a05b824195cc7f0db` |
| `sources/data/XML files/MOOC8_4.01-123_v3.1.00.xml` | `f7882cea914ae3c103ad73acb5cbcd0da07b80854b8a2b2ae95201ee9edbcfb3` |
| `sources/data/XML files/MOOC8_40.01-68_v3.2.00.xml` | `275d4120f60d970b3b71e762f0c8b1b625b718d59ea47a6bcdfd944512f375bc` |
| `sources/data/XML files/MOOC8_41.50_52_v2.0.xml` | `62d679fab89a5a51a87bdd57e10e7a5c5390ec2f7a1ad23a99fecde94678f902` |
| `sources/data/XML files/MOOC8_42.1-50_v2.1.xml` | `0d4ac979de939b88e6e13c1d49dc06483d5191e5142224ee1c1da31e7079bf6e` |
| `sources/data/XML files/MOOC8_43.01-48_v2.0.xml` | `09500212fcdc611631001bf68ca73f5113be186af6182a77c9c939302e446aa3` |
| `sources/data/XML files/MOOC8_44.01-74_v2.0.xml` | `b9abbefc6d4da5c0161951e28c43506a99cd7af468baee57ed713916f9631607` |
| `sources/data/XML files/MOOC8_45.01-140_v2.0.xml` | `b2207e8caf9eafe756ab2d7a5ebd6debe5ff7d886a57206f2ae9901f3aea8b17` |
| `sources/data/XML files/MOOC8_46.01-104_v5.0.xml` | `8721a9072d90f3cafb28108c684726390c6777d7a5bb8b5e2e6114cd8f6e3316` |
| `sources/data/XML files/MOOC8_47.01-50_v2.0.xml` | `3c39c08af851322240c560f4f1bc7e7e0f8baa7f0401b595b98b12529cb773aa` |
| `sources/data/XML files/MOOC8_48.01-43_v2.0.xml` | `d423ff8686a48ace91cffa568a7d7736930f08440090c208c1ff88e5e1f7a7b1` |
| `sources/data/XML files/MOOC8_49.01-81_v2.0.xml` | `d59e18eba279600a1b423414d7dc8945856e565a1d2fb32586ae099411f05372` |
| `sources/data/XML files/MOOC8_5.01-144_v3.1.00.xml` | `60f0961b5bafeaf4cbff498a667fb2a018496a33026e4886155e0a5ad1fffa39` |
| `sources/data/XML files/MOOC8_50.01-106_v3.0.xml` | `6a91de036d484eee651f12021bc79533baea0182148f38bd8d0df96dc2e636fd` |
| `sources/data/XML files/MOOC8_51.01-50_v2.0.xml` | `95c71d42f99e82074e19cc2bc6017923aa08693349a5c472f8e981cf82cfb09f` |
| `sources/data/XML files/MOOC8_52.01-55_v2.0.xml` | `2e52669c9024e98a63212516937d429f18588771aaf66296a2cae08bf22a4e27` |
| `sources/data/XML files/MOOC8_53.01-64_v2.0.xml` | `bd8013b313bb7c745b816529b9fa8ad55cccce6d40c3711c42323da9ac60dd92` |
| `sources/data/XML files/MOOC8_54.01-34_v2.0.xml` | `ea638c7f13c92ddacbfee52632a33cc2d8e42e61ba7c825c3b35a4cae37d21f7` |
| `sources/data/XML files/MOOC8_55.01-51_v2.0.xml` | `9389d1a68d51b0ff29c846865969feddce16aa4e241ceb3b4d2e7438229915c6` |
| `sources/data/XML files/MOOC8_56.01-58_v2.0.xml` | `cf9805bfae21fece5d1979b7dff8dae9fbe117fb9256d14c650a7a39b597b39c` |
| `sources/data/XML files/MOOC8_57.01-50_v2.0.xml` | `925d874efd29df79c2d0217340da1829fbd551fe5cf35c5d85b635e42555af76` |
| `sources/data/XML files/MOOC8_58.01-49_v2.0.xml` | `ab5bc57e5b40741741c9c201781d3c440441dbf58621e0828540d29540aae1b8` |
| `sources/data/XML files/MOOC8_59.01-54_v2.0.xml` | `8fa50e71573af4d9cd5091d8db1ea23cdf6766175693fe0528f9c6298df64f3b` |
| `sources/data/XML files/MOOC8_6.01-138_v3.3.00.xml` | `0b690abde9e87b6cb061e91c33af2daeb9f5560016a6409084a133002b047bf6` |
| `sources/data/XML files/MOOC8_60.01-26_v2.0.xml` | `d5a8fcded197cb44bb139e962983c4cd644f96672bbe55023399346a85b939d1` |
| `sources/data/XML files/MOOC8_61.01-32_v2.0.xml` | `a9da2be8d48ff33b2c4a50c9b8e3272d0faff76761d809eb833b74e42ac6955f` |
| `sources/data/XML files/MOOC8_62.01-23_v2.0.xml` | `bacc8dab709a94ac63405ca54a277cfa98db88be9e3d4d28dab9a2f0f17edcef` |
| `sources/data/XML files/MOOC8_63.01-28_v2.0.xml` | `2b5710b323c0e0926f37a3b756e6993bde219dafff29fa80c9379d3efa47ff7e` |
| `sources/data/XML files/MOOC8_64.01-23_v2.0.xml` | `189caa4be6166d28da4e08c5629c23ddc10b4ca360606f967b3eb08500403fc4` |
| `sources/data/XML files/MOOC8_65.01-17_v2.0.xml` | `3d6aee226a97547d712eb0d4207f76ca78dc73c6a8642f499798e509b8b458f6` |
| `sources/data/XML files/MOOC8_66.01-26_v3.0.xml` | `3652bf19304f64e2cdb9464e1ca4062ac35553c3f31afabe7e7e0adf0e06cfe0` |
| `sources/data/XML files/MOOC8_67.01-34_v2.0.xml` | `782e755f7e49d0a5b67aabe6f534447f41cd01c4d8395bc61d2bab8ea7446b68` |
| `sources/data/XML files/MOOC8_68.01-34_v2.0.xml` | `d58fc7eeef6e7dd26e5aea387e9ff62ea8e7ab53dc03568f2541bfc8c94b842b` |
| `sources/data/XML files/MOOC8_69.01-39_v2.0.xml` | `737869dfdd1229791771c1051016c483f408745e084befe3305a4b039c2ac696` |
| `sources/data/XML files/MOOC8_7.01-72_v3.2.00.xml` | `48bcc9b8de46d15b9dd9bdaf111e5ee511dea1504433f310c8c832bdc0a68bde` |
| `sources/data/XML files/MOOC8_70.01-52_v2.0.xml` | `ee7c4a9d2c2bacb6f6d5dc7facc8226487e859cd970b78ab19039b609614e853` |
| `sources/data/XML files/MOOC8_71.01-33_v2.0.xml` | `f61fb6f13d346c55f7192948f25911b8615f33d1bbbf5cbc86fdc0e0817e2b11` |
| `sources/data/XML files/MOOC8_72.01-24_v2.0.xml` | `5a0233ebbd2c241833bd761d16248e40fe514617ef31bd5865df2c6942b03ff7` |
| `sources/data/XML files/MOOC8_73.01-38_v2.0.xml` | `3348759edb43d652014a238cb00e344ceb7a8e2c8e3aa066ba190bc42e8f9b8e` |
| `sources/data/XML files/MOOC8_74.01-30_v2.0.xml` | `cd7908a6fc768c9fdab9499e3b17283aa64e3c048cd36b981fda1833be54a362` |
| `sources/data/XML files/MOOC8_75.01-70_v2.0.xml` | `cd992daec6f89e0c66963b73643f8a4d148eca0e1fae2f5176a0d5c1976ef7ab` |
| `sources/data/XML files/MOOC8_8.01-48_v3.2.00.xml` | `f6fb13508813b51ce108c5089dd3b301a5dd678556b023d0d99494c04dc8911c` |
| `sources/data/XML files/MOOC8_9.01-53_v3.2.00.xml` | `8f761650811f7b04920642f9f420e9d8678963f7763af25a4b0a5fc15c308867` |
| `sources/data/Early Cape District - Including Full Index for Hague & Cape Archives September 2024.xlsx` | `715460c000b6300863c28875d032a81e98399ee05d2c28f6ccd479603196b8b0` |
| `sources/data/Stellenbosch-Drakenstein Earlier Opgaafrolle - Hague & Cape Archives - including Full Indexes June 2022.xlsx` | `bfd0d9fc12ab3617f31b92cb01254a5db5f67e4f61572ece63366ab0c4b054d6` |
| `sources/data/vc_datastel (v3).xlsx` | `55e29b418612b4d2d4f5a1709a7dd80416e96eeaec3d29816b0e7eeec38a82d4` |
| `THoTF/notes/validation_79_for_student_karli.xlsx` | `2de759e116cc8a47065bba739b3757bc7c5114ea3c5e849e23c5072706e07ff6` |
| `THoTF/validation_79_for_student.xlsx` | `00d9a425d0f806af446558c56b741dccd34221142075989e34d0541f0e82505c` |
| `Fourie_Households/Data/v1/opgaaf_tax_units_clean_v1.csv` | `a3bb7252e490029cd6a9e86f9b73a8b9e7ccb3675d30f5f3e45af2158bc8fe32` |
| `Fourie_Households/Data/v1/census_schema_registry_v1.csv` | `7b8520e937d8c63db0e63e0df9b86d1496a0bfaacecc3312600a53fe4f8a0aac` |
| `Fourie_Households/Data/v1/census_sheet_registry_v1.csv` | `77058d79080dbead7a76331500cb5c51553135a90fba05ce38ace7c2261d5a22` |

## Licence

Code and derived data: CC BY 4.0 (see `LICENSE`). Quoted source passages remain subject to the custodians' terms.
