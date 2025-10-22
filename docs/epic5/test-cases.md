# E2E Test Cases for Connection Request & Network Features

## Accept Request Test Cases (3)

### TC-01: Accept Single Pending Request

- User accepts one pending connection request
- Verify connection status changes from 'P' to 'A' in connections.txt
- Verify confirmation message displays requester's full name

### TC-02: Accept Multiple Sequential Requests

- User accepts 3-4 pending requests one after another
- Verify all connections update to 'A' status
- Verify network view shows all accepted connections with profile details

### TC-03: Accept Request After Viewing Profile

- User searches for someone who sent them a request
- Views their complete profile
- Accepts the pending request from that user
- Verify connection establishes correctly

## Reject Request Test Cases (3)

### TC-04: Reject Single Pending Request

- User rejects one pending connection request
- Verify request is completely removed from connections.txt
- Verify WS-CONNECTIONS-COUNT decrements
- Verify rejection message displays requester's full name

### TC-05: Reject All Multiple Requests

- User rejects 4 consecutive pending requests
- Verify all requests removed from connections table
- Verify array shifting works correctly (no gaps/corruption)
- Verify "no pending requests" message displays afterward

### TC-06: Reject Then Send New Request to Same User

- User A rejects User B's request
- User A later sends new request to User B
- Verify old request fully removed before new one created
- Verify new request has correct sender/receiver roles

## Mixed Scenario Test Cases (4)

### TC-07: Accept Some, Reject Others

- User has 4 pending requests and accepts 2, rejects 2
- Verify only accepted connections appear in network view
- Verify rejected requests are removed
- Verify connections.txt contains accurate final state

### TC-08: Invalid Choice During Request Review

- User enters invalid input (not 1 or 2) when reviewing request
- Verify error message displays
- Verify request status unchanged (still 'P')
- Verify user can retry with valid input

### TC-09: Accept Request and View Network Immediately

- User with existing connections accepts a new request
- Immediately views network
- Verify new connection appears with correct profile data
- Verify connection count increments correctly

### TC-10: View -> Accept -> View -> Reject -> view

- User views pending requests, accepts one, views network, rejects another, views network again
- Verify each step reflects correct state changes
