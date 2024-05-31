import { faker } from '@faker-js/faker'
import { Button } from 'antd'
import { ProtectedTemplate } from '../templates/ProtectedTemplate'
import { http } from '../../http'

export function MainPage() {
  return (
    <ProtectedTemplate title="Page title">
      <div>
        <p>
          <Button onClick={() => http.post(`/Something/Test`)}>Backend test request</Button>
        </p>
        <p>{faker.lorem.sentence(1000)}</p>
      </div>
    </ProtectedTemplate>
  )
}
