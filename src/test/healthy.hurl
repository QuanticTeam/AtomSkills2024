
GET https://localhost:5004/login
HTTP/1.1 200
[Asserts]
certificate "Subject" == "CN = localhost"
xpath "string(/html/head/title)" contains "Welding and Sons - AtomSkills2024"


