Feature: Use git-backup to create backup and access it
  In order to replace current buffer with backup
  As a user
  I run git-backup command

  Background: I created a file and did 3 changes
    Given I open file "/tmp/test/file"
    Then I should be in buffer "file"
    Then I insert "First change" and save
    Given I clear the buffer
    Then I insert "Second change" and save
    Given I clear the buffer
    Then I insert "Third change" and save

  Scenario: I open backup in new buffer
    When I open backup number "3"
    Then I should see "First change"
    Then I should be in buffer name matching regexp "/tmp/test/file.*"

  Scenario: I replace buffer content with backup
    When I replace current buffer with backup number "3"
    Then I should see "First change"
    Then I should be in buffer name matching regexp "file"
