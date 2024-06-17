
GET https://localhost:5004
HTTP/1.1 304
[Asserts]
certificate "Subject" == "CN = localhost"
xpath "string(/html/head/title)" contains "Welding and Sons - AtomSkills2024"
