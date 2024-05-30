import { faker } from '@faker-js/faker'
import { Button } from 'antd'
import { PageTemplate } from '../templates/ProtectedTemplate'

export function MainPage() {
  return (
    <PageTemplate title="Page title">
      <div>
        <p>
          <Button
            onClick={() =>
              fetch(
                `${process.env.REACT_APP_BACKEND_ADDR}:${process.env.REACT_APP_BACKEND_PORT}/Something/Test`,
                {
                  method: 'POST',
                },
              )
            }
          >
            Backend test request
          </Button>
        </p>
        <p>{faker.lorem.sentence(1000)}</p>
      </div>
    </PageTemplate>
  )
}
