import { UserOutlined } from '@ant-design/icons'
import { faker } from '@faker-js/faker'
import { Avatar, Badge, Button, Input, Typography } from 'antd'
import { capitalize } from 'lodash'
import { PageTemplate } from './../templates/PageTemplate'

import colors from '../../colors.json'

const breadcrumbs = [
  { title: 'Header left' },
  ...Array.from({ length: 2 }, () => ({ title: capitalize(faker.hacker.noun()) })),
]

export function MainPage() {
  return (
    <PageTemplate
      breadcrumbs={breadcrumbs}
      content={
        <div>
          <p>
            <Button
              onClick={() =>
                fetch('http://localhost:5002/Something/Test', {
                  method: 'POST',
                })
              }
            >
              Backend test request
            </Button>
          </p>
          <p>{faker.lorem.sentence(1000)}</p>
        </div>
      }
      headerLeft={
        <Input.Search
          className="w-96"
          placeholder={faker.lorem.sentence(5)}
        />
      }
      headerRight={
        <Badge
          color={colors['primary-2']}
          count={12}
          offset={[-10, 0]}
          size="small"
        >
          <Avatar
            size={48}
            icon={<UserOutlined />}
          />
        </Badge>
      }
      title={<Typography.Title>Page title</Typography.Title>}
    />
  )
}
