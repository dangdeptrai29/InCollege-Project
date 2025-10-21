# FC4-229 — Persistence Test Artifacts

## Sample Input / Output Pair
- `io/InCollege-Input-Week6-Persistence.txt` drives two consecutive postings (software + data internships) in a single session.
- Running the program with that file copied to `io/InCollege-Input.txt` generates `io/InCollege-Output-Week6-Persistence.txt`, confirming both success messages with monotonically increasing IDs.

## Verification Flow
1. Back up `io/InCollege-Input.txt` if needed, then copy the persistence input file into place.
2. Execute `./incollege` (all prompts read from the copied input file).
3. Inspect `io/InCollege-Output.txt` (or the saved snapshot in `io/InCollege-Output-Week6-Persistence.txt`) to verify both postings were accepted.
4. Open `data/jobs.txt` and confirm the new postings were appended with sequential IDs that survive subsequent runs (`data/jobs.txt:9` and `data/jobs.txt:10`).
5. Restore the original input file once validation is complete.

## Notes for QA
- The sample leaves the application without additional input; the program handles EOF gracefully after the second posting.
- Re-running the sample continues incrementing IDs, providing an easy check that persistence works across sessions.
