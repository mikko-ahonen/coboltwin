# Claude instructions for coboltwin

- This file is generated. Add project-specific instructions to `CLAUDE.local.md`.



## Type
Legacy



## Environments



## Key Operations


- COBOL: `cobc -x program.cob` - Compile COBOL program
- COBOL: `cobc -x -free program.cob` - Compile free-format COBOL
- Java/Maven: `mvn compile` - Compile Java project
- Java/Maven: `mvn test` - Run Java tests
- Java/Maven: `mvn package` - Build JAR
- Python: `python script.py` - Run Python script
- Python: `pytest` - Run Python tests


## Structure and files

Do not create extra directory structure. Store directories at the repository root. See README.md for organzation.

- All documentation: `docs/` directory    # For all concise, clear, documentation
- Temporary files: `tmp/` directory       # For example test reports, plans, bug fix reports


## Development Phases

In general, we work on a specific phase at the time. Phase describes the primary focus, it is informational not normative; anything can be changed at any
time and we can jump back to a phase. Phase may be related to the whole project or later iteratively for a new feature we are working on 
for existing projects.

The phases are:

concept → design → prototype → implement → testing → userguide → e2e → landing → training

There are specific instructions for each phase. Look the corresponding instructions when doing such changes.

- `docs/concept/concept.md` -- Concept
- `docs/design/design.md` -- Design
- `docs/prototype/prototype.md` -- Prototyping
- `docs/implementation/implementation.md` -- Implementation and unit testing
- `docs/testing/testing.md` -- Comprehensive integration tests
- `docs/userguide/userguide.md` -- Creating or updating the user guide for the project or feature
- `docs/e2e/e2e.md` -- Comprehensive e2e test suite
- `docs/landing/landing.md` -- Landing page, including short landing page videos
- `docs/training/training.md` -- Longer training videos and other material

## Tickets

Keep all features, todos, bugs etc. in GitHub tickets. Use skill 'ticket' to access tickets. They may be supplied by users, developers,
or created by you for example based on Sentry issues.


- Compile COBOL programs after changes to check for errors.
- Build and run Java/Maven tests after changes.
- Run Python tests with `pytest` after changes.
